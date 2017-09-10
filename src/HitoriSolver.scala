import java.io.{File, PrintWriter}
import javax.swing.plaf.metal.MetalBorders.TableHeaderBorder

import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

/**
  * Created by kjell on 06.09.2017.
  */
object HitoriSolver {


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
  class HBoard(ROWS: List[HItem], VALID: Boolean = true) {
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

    var unsolvedItems = board.items.filter(m => m.state == "U");

    var b = board

    println("Entering phase 1...")

    // Phase 1 search

    b = phase1(b)

    //if (isSolved(b))
    // return b

    printBoard(b)

    println("Entering phase 2...")
    unsolvedItems = b.items.filter(m => m.state == "U");
    println("Number of unsolved items: " + unsolvedItems.length)

    val flag = rule3(b)
    println("Is connected after 1: " + flag)

    // Phase 2 search
    b = phase2(b)

    var flag2 = rule3(b)
    println("Is connected after 2: " + flag2)
    flag2 = rule3(b)
    println("Is consistent after 32: " + flag2)

    if (isSolved(b)) return b

    printBoard(b)

    println("Entering phase 3...")
    unsolvedItems = b.items.filter(m => m.state == "U");
    println("Number of unsolved items: " + unsolvedItems.length)

    // Phase 3 search
    b = backtracking(b)


    val flag3 = rule3(b)
    println("Is connected after 3: " + flag3)

    val flag4 = rule2(b)
    println("Is consistent after 3: " + flag4)

    unsolvedItems = b.items.filter(m => m.state == "U");
    println("Cant find more solutions!")
    println("Number of unsolved items: " + unsolvedItems.length)

    return b
  }


  /* Phase-1 functions */

  def phase1(board: HBoard): HBoard = {

    var b = board

    printBoard(b)

    val unsolvedItemsBefore = board.items.filter(m => m.state == "U");

    b.items.foreach(i => b = phase1Search(b, i));

    val unsolvedItemsAfter = board.items.filter(m => m.state == "U");

    if (unsolvedItemsBefore.length == unsolvedItemsAfter.length) {
      return b
    }
    else {
      phase1(b)
    }
  }

  def phase1Search(board: HBoard, item: HItem): HBoard = {

    var b = board

    printBoard(b)

    val row = board.items.filter(_.y == item.y);
    val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    val col = board.items.filter(_.x == item.x);
    val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    val conflicts = conflictsRow ::: conflictsCol

    //val conflicts: List[HItem] = board.items.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state != "B").map(_._1).filter(i => i.x == item.x || i.y == item.y)

    // Pattern 1 - Sandwich
    conflicts.foreach(i => b = patternSandwich(b, item, i))

    // Pattern 2 - Double corner
    conflicts.foreach(i => b = patternDoubleCorner(b, item, i))

    // Pattern 4 - Uniqueness
    //b = checkForUniqueness(b, item)

    // Pattern 4 - Floodtest
    // b = flood(b,item)

    return b
  }

  def patternDoubleCorner(board: HBoard, itemA: HItem, itemB: HItem): HBoard = {
    println("Double corner rule")
    val f = itemA match {
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
    return f;
  }

  def patternSandwich(board: HBoard, itemA: HItem, itemB: HItem): HBoard = {
    println("Sandwich corner rule")
    itemA match {
      case i if itemB.x == i.x + 2 => setCellWhite(board, i.x + 1, i.y)
      case i if itemB.x == i.x - 2 => setCellWhite(board, i.x - 1, i.y)
      case i if itemB.y == i.y + 2 => setCellWhite(board, i.x, i.y + 1)
      case i if itemB.y == i.y - 2 => setCellWhite(board, i.x, i.y - 1)
      case _ => board
    }

  }


  def checkForPath2(board: HBoard, itemA: HItem): HBoard = {

    if (itemA.state == "B") return board;

    val neighbours = getAllNeighbours(board, itemA)

    var conflicts = neighbours.filter(i => i != null).filter(i => i.state == "B")

    neighbours.foreach(i => if (i == null) conflicts = conflicts ::: List(new HItem("F", -1, -1, "B")))

    if (conflicts.length < 3) return board

    val nonConflict = neighbours.filter(i => i != null).filter(i => i.state == "U");

    if (nonConflict.isEmpty) return board

    return setCellWhite(board, nonConflict.head.x, nonConflict.head.y)

  }

  def checkForUniqueness(board: HBoard, item: HItem): HBoard = {


    printBoard(board)
    if (item.state != "U") return board

    val row = board.items.filter(_.y == item.y).filter(i => i.state != "B")
    var conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).map(_._1)

    val col = board.items.filter(_.x == item.x).filter(i => i.state != "B")
    var conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).map(_._1)

    conflictsRow = conflictsRow.filter(i => i.x != item.x)
    conflictsCol = conflictsCol.filter(i => i.y != item.y)

    if (conflictsRow.isEmpty && conflictsCol.isEmpty)
      return setCellWhite(board, item.x, item.y)

    return board
  }

  def blackLeadsToInvalid(board: HBoard): HBoard = {
    return board
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

      if (itemToFix.y != item.y) {
        b = setCellBlack(b, itemToFix.x, itemToFix.y);
      }
    }

    // All elements in the white cells's col
    val col = board.items.filter(m => m.x == item.x);
    val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    for (i <- conflictsCol.indices) {
      val itemToFix = conflictsCol(i);
      if (itemToFix.y != item.y) {
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

    if (item.state == "W")
      println("Violating rule!")

    if (item.state != "U") return board;

    val b = new HBoard(board.items.map(m => if (m.x == xPos && m.y == yPos) new HItem(item.value, item.x, item.y, "B") else m));
    return setEdgesWhite(b, xPos, yPos)
  }

  def setCellWhite(board: HBoard, xPos: Int, yPos: Int): HBoard = {
    val item = getCellXY(board, xPos, yPos);

    if (item.state != "U") return board;

    val b = new HBoard(board.items.map(m => if (m.x == xPos && m.y == yPos) new HItem(item.value, item.x, item.y, "W") else m));

    return setDuplicatesBlack(b, xPos, yPos);
  }


  /* Phase-2 functions */

  def phase2(board: HBoard): HBoard = {

    var b = board
    var field = board

    var dup = unsolvedDuplicates(b)

    var flag = true
    var break = false

    while (flag) {

      flag = false
      break = false

      for (i <- dup) {

        if (!break) {
          field = setCellBlack(field, i.x, i.y)
          field = flood(field, i)
          printBoard(field)

          val rule_1 = rule1(field)
          val rule_2 = rule2(field)
          val rule_3 = rule3(field)

          if (!rule_2 || !rule_3 || !rule_1) {
            field = b
            field = setCellWhite(field, i.x, i.y)
            field = flood(field, i)
            dup = unsolvedDuplicates(field)
            b = field
            flag = true
            break = true
          }
          else
            field = b
        }
      }
    }

    field.items.foreach(i => field = checkForUniqueness(field, i))
    printBoard(field)

    return field
  }

  def phase2Search(board: HBoard): HBoard = {

    val unsolvedBefore = board.items.filter(i => i.state == "U")

    var b = phase2(board)

    val unsolvedAfer = board.items.filter(i => i.state == "U")

    if (unsolvedBefore != unsolvedAfer) {
      printBoard(b)
      b = phase2Search(b)
    }


    return b
  }

  def duplicates(board: HBoard): List[HItem] = {

    val boardSize = (Math.sqrt(board.items.length) - 1).toInt

    var duplicates: List[HItem] = List()

    for (index <- 0 to boardSize) {

      val item = board.items.filter(i => i.x == index && i.y == index).head

      val row = board.items.filter(_.y == item.y);
      val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).map(_._1)

      val col = board.items.filter(_.x == item.x);
      val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).map(_._1)

      duplicates = duplicates ::: conflictsRow ::: conflictsCol
    }

    return duplicates

  }

  def unsolvedDuplicates(board: HBoard): List[HItem] = {
    return duplicates(board).filter(i => i.state == "U")
  }

  def isConsistentOld(board: HBoard): Boolean = {

    val dup = duplicates(board).filter(i => i.state == "W")


    for (item <- dup) {

      val row = dup.filter(_.y == item.y);
      val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).map(_._1).filter(i => i.x != item.x && i.y != item.y)

      if (conflictsRow.length > 0) return false

      val col = board.items.filter(_.x == item.x);
      val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).map(_._1).filter(i => i.x != item.x && i.y != item.y)

      if (conflictsCol.length > 0) return false

    }

    return true

  }

  def isConsistent(board: HBoard): Boolean = {

    if (rule2(board) && rule3(board))
      return true

    return false
  }

  def rule2(board: HBoard, item: HItem): Boolean = {

    val duplicatesRow = board.items.filter(m => m.value == item.value).filter(m => m.y == item.y && m.x != item.x).filter(m => m.state == "W")
    val duplicatesCol = board.items.filter(m => m.value == item.value).filter(m => m.y != item.y && m.x == item.x).filter(m => m.state == "W")

    if (duplicatesCol.nonEmpty || duplicatesRow.nonEmpty)
      return false


    return true

  }

  def rule2(board: HBoard): Boolean = {

    for (item <- board.items) {
      val duplicatesRow = board.items.filter(m => m.value == item.value).filter(m => m.y == item.y && m.x != item.x).filter(m => m.state == "W")
      val duplicatesCol = board.items.filter(m => m.value == item.value).filter(m => m.y != item.y && m.x == item.x).filter(m => m.state == "W")

      if (duplicatesCol.nonEmpty || duplicatesRow.nonEmpty)
        return false

    }

    return true
  }

  def rule1(board: HBoard): Boolean = {
    for (item <- board.items) {
      val duplicatesRow = board.items.filter(m => m.value == item.value).filter(m => m.y == item.y && m.x != item.x).filter(m => m.state == "W")
      val duplicatesCol = board.items.filter(m => m.value == item.value).filter(m => m.y != item.y && m.x == item.x).filter(m => m.state == "W")

      for (i <- duplicatesRow) {
        if (isNextToDuplicate(item, i) != "F")
          return false
      }

      for (i <- duplicatesCol) {
        if (isNextToDuplicate(item, i) != "F")
          return false
      }

    }

    return true
  }

  def flood(board: HBoard, item: HItem): HBoard = {
    if (rule3(board))
      return board

    return setCellWhite(board, item.x, item.y)
  }

  def rule3(board: HBoard): Boolean = {
    val traversableItemsFirst = board.items.filter(i => i.state != "B")
    var traversableItems = board.items.filter(i => i.state != "B")

    var queue = Queue[HItem]()
    val visited = HashSet[HItem]()

    queue += traversableItems(0)

    while (!queue.isEmpty) {
      val item = queue.dequeue()

      traversableItems = traversableItems.filter(i => i.x != item.x && i.y != item.y)

      val neighbours = getAllNeighbours(board, item).filter(i => i != null).filter(i => i.state != "B")

      neighbours.foreach(i => if (visited.add(i)) {
        queue += i
      })
    }

    // println("Visited: " + visited.size)
    // println("Total: " + traversableItemsFirst.length)
    if (visited.size == traversableItemsFirst.length)
      return true

    return false
  }

  /* Phase-3 functions */

  def phase3(board: HBoard): HBoard = {

    return phase3Search(board);
  }

  def phase3Search(board: HBoard): HBoard = {

    var backup = board

    var b = board

    var flag = true


    while (flag) {

      if (isSolved(b))
        flag = false

      var duplicates = b.items.filter(i => i.state == "U")

      for (item <- duplicates) {

        b = setCellBlack(b, item.x, item.y)
        b = phase2(b)

        val consistent = rule2(b, item)
        val isConnected = rule3(b)

        printBoard(b)

        if (!consistent || !isConnected) {
          b = backup
          b = setCellWhite(b, item.x, item.y)
          b = phase2(b)
          backup = b
          flag = true
        }
        else {
          //b = backup
        }
      }

      if (duplicates.isEmpty && !isSolved(b))
        b = backup


    }

    return b

    /*

      for (i <- duplicates) {

        duplicates = b.items.filter(i => i.state == "U")
        printBoard(b)
        println("Duplicates: " + duplicates.length)

        if (isSolved(b)) {
          return b
        }

        b = setCellBlack(b, i.x, i.y)
        b = phase2(b)
        b = phase3Search(b)

        val consistent = isConsistent(b)
        val isConnected = isPathConnected(b)

        if (!consistent || !isConnected) {
          b = backup
          b = setCellWhite(b, i.x, i.y)
          b = flood(b, i)
          b = phase2(b)
          backup = b
          return phase3Search(b)
        }
        else
          b = backup
  */
  }


  def backtracking(board: HBoard): HBoard = {

    var b = board
    var backup = board


    while (true) {

      var unsolvedItems = board.items.filter(m => m.state == "U")

      if (isSolved(b))
        return b

      val item = unsolvedItems.head

      for (i <- unsolvedItems) {
        b = setCellBlack(b, i.x, i.y)
        b = phase2(b)

        if (!rule3(b) || !rule2(b, i)) {
          b = backup
          b = setCellWhite(b, i.x, i.y)
          b = phase2(b)
          backup = b
          unsolvedItems = board.items.filter(m => m.state == "U")
        }
        else {
          backup = b
        }
      }

    }

    return board

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

  def isSolved(board: HBoard): Boolean = {
    val consistent = isConsistent(board)
    val isConnected = rule3(board)
    val unsolvedItems = board.items.filter(m => m.state == "U");

    if (consistent && isConnected && unsolvedItems.length < 1)
      return true

    return false
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


}