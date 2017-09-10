import java.io.{File, PrintWriter}
import javax.swing.plaf.metal.MetalBorders.TableHeaderBorder

import HitoriSolverEngine.isInCorner

/**
  * Created by kjell on 06.09.2017.
  */
object HitoriSolverEngine {


  /**
    * Represents a square or element in a hitori board
    *
    * @param VALUE The value (number) of the square
    * @param X     The x-position of the square in the board (Relative to the upper left corner)
    * @param Y     The y-position of the square in the board (Relative to the upper left corner)
    * @param STATE
    */
  class HItem(VALUE: String, X: Int, Y: Int, STATE: String = "U") {
    val value = VALUE;
    val x = X;
    val y = Y;
    val state = STATE;
  }

  /**
    * Represents a hitori board
    * Consists of a single list of HItems (Squares)
    *
    * @param ROWS All rows in the board
    */
  class HBoard(ROWS: List[HItem], VALID:Boolean = true) {
    val items = ROWS
    val valid = VALID
  }


  def main(args: Array[String]) {

    val inputPath = args(0)
    val outputPath = args(1)

    println("Input path: " + inputPath)
    println("Output path: " + outputPath)

    val board = loadGameFromFile(inputPath);

    val startTime = System.currentTimeMillis()

    val solvedPuzzle = this.solvePuzzle(board);
    val endTime = System.currentTimeMillis();

    printBoard(solvedPuzzle)

    println("Solved puzzle in: " + (endTime - startTime) + " milliseconds")
  }


  /* General solving functions */

  /**
    * Main function of control
    *
    * @param board The Hitori board to solve
    * @return A hitori board with one or more cells solved
    */
  def solvePuzzle(board: HBoard): HBoard = {

    val unsolvedItems = board.items.filter(m => m.state == "U");

    if (unsolvedItems.isEmpty) {
      println("Yay! Solved puzzle!")
      return board
    }

    var b = board

    println("Entering phase 1...")

    // Phase 1 search
    b.items.foreach(i => b = phase1Search(b, i));

    println("Entering phase 2...")

    // Phase 2 search
    b.items.foreach(i => b = phase2Search(b, i))

    println("Entering phase 3...")

    // Phase 3 search
    b = phase3Search(b)

    //val b = this.lookForConflicts(board);

    val unsolvedItemsNew = b.items.filter(m => m.state == "U");

    if (unsolvedItems.length == unsolvedItemsNew.length) {
      println("Cant find more solutions!")
      println("Number of unsolved items: " + unsolvedItemsNew.length)
      return b
    }
    else {
      solvePuzzle(b)
    }


  }


  /* Phase-1 functions */

  def phase1Search(board: HBoard, item: HItem): HBoard = {

    val row = board.items.filter(_.y == item.y);
    val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    val col = board.items.filter(_.x == item.x);
    val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    var b = board;

    if (conflictsRow.isEmpty && conflictsCol.isEmpty)
      return setCellWhite(board, item.x, item.y)


    if (conflictsRow.length > 1) {

      val itemB = conflictsRow(1);

      // Search A
      b = solveDoubleCorner(b, item, itemB)

      // Search B

      // Search C
      b = solveSandwich(b, item, itemB)

      // Search D

    }

    if (conflictsCol.length > 1) {

      val itemB = conflictsCol(1);

      // Search A
      b = solveDoubleCorner(b, item, itemB)

      // Search B

      // Search C
      b = solveSandwich(b, item, itemB)


      // Search D
    }

    b = checkForNeighbour(b, item)

    printBoard(b)

    return b;
  }

  def solveDoubleCorner(board: HBoard, itemA: HItem, itemB: HItem): HBoard = {

    itemA match {
      case i if isInCorner(board, i) == "TL" && isNextToDuplicate(i, itemB) == "R" => setCellWhite(board, i.x, i.y + 1)
      case i if isInCorner(board, i) == "TL" && isNextToDuplicate(i, itemB) == "B" => setCellWhite(board, i.x + 1, i.y)
      case i if isInCorner(board, i) == "TR" && isNextToDuplicate(i, itemB) == "L" => setCellWhite(board, i.x, i.y + 1)
      case i if isInCorner(board, i) == "TR" && isNextToDuplicate(i, itemB) == "B" => setCellWhite(board, i.x - 1, i.y)

      case i if isInCorner(board, i) == "BL" && isNextToDuplicate(i, itemB) == "U" => setCellWhite(board, i.x + 1, i.y)
      case i if isInCorner(board, i) == "BL" && isNextToDuplicate(i, itemB) == "R" => setCellWhite(board, i.x, i.y - 1)
      case i if isInCorner(board, i) == "BR" && isNextToDuplicate(i, itemB) == "U" => setCellWhite(board, i.x - 1, i.y)
      case i if isInCorner(board, i) == "BR" && isNextToDuplicate(i, itemB) == "L" => setCellWhite(board, i.x, i.y - 1)


      case i if isInCornerPlusOne(board, i) == "TLR" && isNextToDuplicate(i, itemB) == "B" => setCellWhite(board, i.x - 1, i.y + 1)
      case i if isInCornerPlusOne(board, i) == "TLB" && isNextToDuplicate(i, itemB) == "R" => setCellWhite(board, i.x + 1, i.y - 1)

      case i if isInCornerPlusOne(board, i) == "TRL" && isNextToDuplicate(i, itemB) == "B" => setCellWhite(board, i.x + 1, i.y + 1)
      case i if isInCornerPlusOne(board, i) == "TRB" && isNextToDuplicate(i, itemB) == "L" => setCellWhite(board, i.x - 1, i.y - 1)

      case i if isInCornerPlusOne(board, i) == "BLT" && isNextToDuplicate(i, itemB) == "R" => setCellWhite(board, i.x + 1, i.y + 1)
      case i if isInCornerPlusOne(board, i) == "BLR" && isNextToDuplicate(i, itemB) == "T" => setCellWhite(board, i.x - 1, i.y - 1)

      case i if isInCornerPlusOne(board, i) == "BRT" && isNextToDuplicate(i, itemB) == "L" => setCellWhite(board, i.x - 1, i.y + 1)
      case i if isInCornerPlusOne(board, i) == "BRL" && isNextToDuplicate(i, itemB) == "T" => setCellWhite(board, i.x + 1, i.y - 1)

      case _ => board
    }
  }

  def checkForNeighbour(board: HBoard, item: HItem): HBoard = {

    if (item.state == "B") return board


    var b = board

    val neighboursRow = getAllNeighbours(board, item).filter(i => i != null).filter(i => i.y == item.y)
    neighboursRow.foreach(i => b = solveNeighbourRow(b, item, i))

    val neighboursCol = getAllNeighbours(board, item).filter(i => i != null).filter(i => i.x == item.x)
    neighboursCol.foreach(i => b = solveNeighbourCol(b, item, i))

    return b
  }

  def solveNeighbourCol(board: HBoard, itemA: HItem, itemB: HItem): HBoard = {

    if (itemA.value != itemB.value) return board

    var b = board

    val conflictCol = findNeighbourConflictCol(board, itemA, itemB)

    if (conflictCol.length > 0)
      conflictCol.foreach(i => b = setCellBlack(b, i.x, i.y));

    return b
  }

  def solveNeighbourRow(board: HBoard, itemA: HItem, itemB: HItem): HBoard = {

    if (itemA.value != itemB.value) return board

    var b = board

    val conflictRow = findNeighbourConflictRow(board, itemA, itemB)

    if (conflictRow.length > 0)
      conflictRow.foreach(i => b = setCellBlack(b, i.x, i.y));


    return b
  }

  def findNeighbourConflictRow(board: HBoard, itemA: HItem, itemB: HItem): List[HItem] = {
    val row = board.items.filter(_.y == itemA.y).filter(_.x != itemA.x).filter(_.x != itemB.x).filter(_.value == itemA.value)
    return row.zipWithIndex.filter(_._1.value == itemA.value).map(_._1)
  }

  def findNeighbourConflictCol(board: HBoard, itemA: HItem, itemB: HItem): List[HItem] = {
    val row = board.items.filter(_.x == itemA.x).filter(_.y != itemA.y).filter(_.y != itemB.y).filter(_.value == itemA.value)
    return row.zipWithIndex.filter(_._1.value == itemA.value).map(_._1)
  }

  def solveSandwich(board: HBoard, itemA: HItem, itemB: HItem): HBoard = {

    val bool = isSandwich(board, itemA, itemB);

    if (bool)
      println("Solved sandwitch!")


    itemA match {
      case i if itemB.x == i.x + 2 => setCellWhite(board, i.x + 1, i.y)
      case i if itemB.x == i.x - 2 => setCellWhite(board, i.x - 1, i.y)
      case i if itemB.y == i.y + 2 => setCellWhite(board, i.x, i.y + 1)
      case i if itemB.y == i.y - 2 => setCellWhite(board, i.x, i.y - 1)
      case _ => board
    }

  }

  def isSandwich(board: HBoard, itemA: HItem, itemB: HItem): Boolean = {

    itemA match {
      case i if itemB.x == i.x + 2 => true
      case i if itemB.x == i.x - 2 => true
      case i if itemB.y == i.y + 2 => true
      case i if itemB.y == i.y - 2 => true
      case _ => false
    }

  }


  /* Phase-2 functions */

  def phase2Search(board: HBoard, item: HItem): HBoard = {

    printBoard(board)

    var b = board

    // Search A
    b = checkForPath(b, item)

    // Search B
    b = checkForUniqueness(b, item)

    return b;
  }

  def checkForPath(board: HBoard, itemA: HItem): HBoard = {

    if (itemA.state == "B") return board;

    val neighbours = getAllNeighbours(board, itemA)

    val conflicts = neighbours.filter(i => i != null).filter(i => i.state == "B")

    if (conflicts.length < 3) return board

    val nonConflict = neighbours.filter(i => i != null).filter(i => i.state == "U");

    if (nonConflict.isEmpty) return board

    return setCellWhite(board, nonConflict.head.x, nonConflict.head.y)

  }

  def checkForUniqueness(board: HBoard, item: HItem): HBoard = {

    if (item.state != "U") return board

    val row = board.items.filter(_.y == item.y);
    val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state != "B").map(_._1)

    val col = board.items.filter(_.x == item.x);
    val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state != "B").map(_._1)

    if (conflictsRow.isEmpty && conflictsCol.isEmpty)
      return setCellWhite(board, item.x, item.y)

    return board
  }


  /* Phase-3 functions */

  def phase3Search(board: HBoard): HBoard = {


    val unsolvedItems = board.items.filter(p => p.state == "U")

    println("Unsolved items: " + unsolvedItems.length)

    var b = board

    for (i <- unsolvedItems) {

      printBoard(board)

      val temp = setCellWhite(b,i.x, i.y)
      var temp2 = temp
      temp.items.foreach(i => temp2 = phase2Search(temp2, i))

      val unsolvedItems2 = board.items.filter(p => p.state == "U")
      val diff = unsolvedItems.length - unsolvedItems2.length

      println("Changes phase 3: " + diff)

      b = temp

    }


    return b;
  }


  /* Helper functions */

  def saveSolvedPuzzle(board: HBoard, outputPath: String): Unit = {
    // Solve puzzle and output to file, like so:

    var outputFile = new PrintWriter(new File(outputPath), "UTF-8")

    outputFile.println("b w w w w")
    outputFile.println("W B W B W")
    outputFile.println("W W W W B")
    outputFile.println("W W B W W")
    outputFile.println("B W W W B")

    outputFile.close()
  }

  def isNextToDuplicate(itemA: HItem, itemB: HItem): String = {
    itemA match {
      case i if (itemB.x == i.x + 1 && i.y == itemB.y) => "R" // ItemB is right of itemA
      case i if (itemB.x == i.x - 1 && i.y == itemB.y) => "L" // ItemB is left of itemA
      case i if (itemB.y == i.y + 1 && i.x == itemB.x) => "T" // ItemB is over itemA (Top)
      case i if (itemB.y == i.y - 1 && i.x == itemB.x) => "B" // ItemB is under itemA (Bottom)
      case _ => "F";
    }
  }

  def isInCorner(board: HBoard, itemA: HItem): String = {
    val bs = math.sqrt(board.items.length) - 1;

    itemA match {
      case i if i.x - 1 == -1 && i.y - 1 == -1 => "TL" // TopLeft
      case i if i.x == bs + 1 && i.y - 1 == -1 => "TR" // TopRight
      case i if i.x - 1 == -1 && i.y == bs + 1 => "BL" // BottomLeft
      case i if i.x == bs && i.y == bs => "UL" // BottomRight
      case _ => "F";
    }
  }

  def isInCornerPlusOne(board: HBoard, itemA: HItem): String = {
    val bs = math.sqrt(board.items.length) - 1;

    val i = itemA match {
      case i if i.x - 2 == -1 && i.y - 1 == -1 => "TLR" // TopLeft - right
      case i if i.x - 1 == -1 && i.y - 2 == -1 => "TLB" // TopLeft - bottom
      case i if i.x - 1 == bs && i.y - 1 == -1 => "TRL" // TopRight - left
      case i if i.x == bs && i.y - 2 == -1 => "TRB" // TopRight - left
      case i if i.x - 1 == -1 && i.y == bs - 1 => "BLT" // BottomLeft - top
      case i if i.x - 2 == -1 && i.y == bs + 1 => "BLR" // BottomLeft - right
      case i if i.x == bs && i.y == bs - 1 => "BRT" // BottomRight - top
      case i if i.x == bs - 1 && i.y == bs => "BRL" // BottomRight - left
      case _ => "F";
    }

    return i;
  }

  def getCellXY(board: HBoard, xPos: Int, yPos: Int): HItem = {

    val boardSize = Math.sqrt(board.items.length) - 1

    if (xPos < 0 || yPos < 0 || xPos > boardSize || yPos > boardSize) return null

    return board.items.filter(i => i.x == xPos).filter(i => i.y == yPos).head
  }

  def printBoard(board: HBoard): Unit = {

    val boardSize = math.sqrt(board.items.length) - 1;

    val b = new HBoard(board.items.sortWith(_.x < _.x).sortWith(_.y < _.y));

    println("------------------------------------------------")
    for (y <- 0 to boardSize.toInt) {

      for (x <- 0 to boardSize.toInt) {
        val item = getCellXY(b, x, y)
        print(item.state + " ")
      }

      print("\n")
    }

    println("------------------------------------------------")
  }

  def getAllNeighbours(board: HBoard, itemA: HItem): List[HItem] = {
    val left = getCellXY(board, itemA.x - 1, itemA.y)
    val right = getCellXY(board, itemA.x + 1, itemA.y)

    val up = getCellXY(board, itemA.x, itemA.y - 1)
    val down = getCellXY(board, itemA.x, itemA.y + 1)

    return List(left, right, up, down)
  }

  def setDuplicatesBlack(board: HBoard, xPos: Int, yPos: Int): HBoard = {

    // The white cell
    val item = getCellXY(board, xPos, yPos);

    var b = board;

    // All elements in the white cells's row
    val row = board.items.filter(m => m.y == item.y);
    val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    for (i <- conflictsRow.indices) {
      val itemToFix = conflictsRow(i);

      if (itemToFix.x != item.x || itemToFix.y != item.y) {
        b = setCellBlack(b, itemToFix.x, itemToFix.y);
      }
    }

    // All elements in the white cells's col
    val col = board.items.filter(m => m.x == item.x);
    val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    for (i <- conflictsCol.indices) {
      val itemToFix = conflictsCol(i);
      if (itemToFix.x != item.x || itemToFix.y != item.y) {
        b = setCellBlack(b, itemToFix.x, itemToFix.y);
      }
    }

    return b;
  }

  def setEdgesWhite(board: HBoard, xPos: Int, yPos: Int): HBoard = {
    val item = getCellXY(board, xPos, yPos);

    var b = board;

    b.items.foreach(m => {
      m match {
        case i if (i.x == (item.x + 1) && i.y == item.y) => b = setCellWhite(b, i.x, i.y)
        case i if (i.x == (item.x - 1) && i.y == item.y) => b = setCellWhite(b, i.x, i.y)
        case i if (i.y == (item.y + 1) && i.x == item.x) => b = setCellWhite(b, i.x, i.y)
        case i if (i.y == (item.y - 1) && i.x == item.x) => b = setCellWhite(b, i.x, i.y)
        case _ => b = b
      }
    });

    return b;
  }

  def setCellBlack(board: HBoard, xPos: Int, yPos: Int): HBoard = {
    val item = getCellXY(board, xPos, yPos);

    if(item.state == "W")
      println("Violating rule!")

    if (item.state != "U") return board;

    val b = new HBoard(board.items.map(m => if (m.x == xPos && m.y == yPos) new HItem(item.value, item.x, item.y, "B") else m));
    return setEdgesWhite(b, xPos, yPos)
  }

  def setCellWhite(board: HBoard, xPos: Int, yPos: Int): HBoard = {
    val item = getCellXY(board, xPos, yPos);

    if(item.state == "B")
      println("Violating rule")

    if (item.state != "U") return board;



    val b = new HBoard(board.items.map(m => if (m.x == xPos && m.y == yPos) new HItem(item.value, item.x, item.y, "W") else m));

    return setDuplicatesBlack(b, xPos, yPos);
  }


  /**
    * Load the file from the specified path
    *
    * @param inputPath The filepath of the board (Relative to the src folder)
    * @return The hitori-board parsed from the given file.
    */
  def loadGameFromFile(inputPath: String): HBoard = {
    val puzzleFile = new File(inputPath)
    val lines = scala.io.Source.fromFile(puzzleFile).mkString.split("\r\n");

    var board: HBoard = new HBoard(List())

    lines.indices.foreach(y => {
      val row = lines(y).split(" ");
      row.indices.foreach(x => board = new HBoard(board.items ::: List(new HItem(row(x), x, y))))
    });

    // TODO: Remove this for better performance!
    // Prints the board (for testing and debugging)
    lines.foreach(i => println(i))

    return board

  }


  /*
def duplicatesInRow(board: HBoard, item: HItem): HBoard = {

  val row = board.items.filter(_.y == item.y);
  val conflicts: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

  if (conflicts.length > 1) {
    val itemB = conflicts(1);
    return solveConflict(board, item, itemB);
  } else if (conflicts.length == 1) {
    return setCellWhite(board, item.x, item.y);
  }
  return board;
}

def duplicatesInCol(board: HBoard, item: HItem): HBoard = {
  val col = board.items.filter(_.x == item.x);
  val conflicts: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

  if (conflicts.length > 1) {
    val itemB = conflicts(1);
    return solveConflict(board, item, itemB);
  } else if (conflicts.length == 1) {
    return setCellWhite(board, item.x, item.y);
  }
  return board;
}
*/

}