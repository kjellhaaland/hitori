import java.io.{File, PrintWriter}
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue


object HitoriSolver
{

  /**
    * Represents a square or element in a hitori board
    *
    * @param VALUE The value (number) of the square
    * @param X     The x-position of the square in the board (Relative to the upper left corner)
    * @param Y     The y-position of the square in the board (Relative to the upper left corner)
    * @param STATE
    */
  class HItem(VALUE: String, X: Int, Y: Int, STATE: String = "U")
  {
    val value = VALUE;
    val x = X;
    val y = Y;
    val state = STATE;
  }

  /**
    * Represents a hitori board
    * Consists of a single list of HItems (Squares)
    *
    * @param CELLS All rows in the board
    */
  class HBoard(CELLS: List[HItem], VALID: Boolean = true)
  {
    val items = CELLS
    val size  = (math.sqrt(this.items.length) - 1).toInt
  }


  def main(args: Array[String])
  {

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

    saveSolvedPuzzle(solvedPuzzle, outputPath)
  }


  /* General solving functions */

  /**
    * Main function of control
    *
    * @param board The Hitori board to solve
    * @return A hitori board with one or more cells solved
    */
  def solvePuzzle(board: HBoard): HBoard =
  {
    var unsolvedItems = board.items.filter(m => m.state == "U");

    var b = board

    if(!isBoardValid(b))
      return b

    println("Entering phase 1...")

    // Phase 1 search

    b = phase1(b)

    if (isConsistent(b))
      return b

    //printBoard(b)
    unsolvedItems = b.items.filter(m => m.state == "U");
    println("Number of unsolved items after phase 1: " + unsolvedItems.length)
    println("Entering phase 2...")

    // Phase 2 search
    b = phase2(b)

    if (isConsistent(b))
      return b

    unsolvedItems = b.items.filter(m => m.state == "U");
    println("Number of unsolved items after phase 2: " + unsolvedItems.length)
    println("Entering phase 3...")

    // Phase 3 search
    b = phase3(b)

    unsolvedItems = b.items.filter(m => m.state == "U");

    println("Number of unsolved items: " + unsolvedItems.length)


    println("Checking for consistency")
    val consistent = isConsistent(b)

    if(consistent)
      return b

    // If the board was inconsistent, return the original board and print error message
    println("The given board has no solution! Exiting....")

    return board
  }


  /* Phase-1 functions */

  def phase1(board: HBoard): HBoard =
  {

    var b = board

    b = patternOneTimesOne(b)

    val unsolvedItemsBefore = board.items.filter(m => m.state == "U");

    b.items.foreach(i => b = phase1Search(b, i));

    val unsolvedItemsAfter = board.items.filter(m => m.state == "U");

    if (unsolvedItemsBefore.length == unsolvedItemsAfter.length)
    {
      b.items.foreach(i => b = checkForUniqueness(b, i))
      return b
    }
    else
    {
      phase1(b)
    }
  }

  def phase1Search(board: HBoard, item: HItem): HBoard =
  {

    var b = board

    //printBoard(b)
    val row = board.items.filter(_.y == item.y);
    val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    val col = board.items.filter(_.x == item.x);
    val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    val conflicts = conflictsRow ::: conflictsCol


    for (i <- conflicts)
    {

      // Pattern 1 - Sandwich
      b = patternSandwich(b, item, i)

      // Pattern 2 - Double corner
      b = patternDoubleCorner(b, item, i)

      // Pattern 3 - Triple corner
      b = patternTripleCorner(b, item, i)

      // Pattern 4 - Quad corner
      b = patternQuadCorner(b, item, i)

      // Pattern 5 - Triple in a row/col
      b = patternTriple(b, item)

      // Pattern 6 - Flipped Triple Corner
      b = patternFlippedTripleCorner(b, item)

      b = patternPairIsolation(b,item)
    }

    return b
  }

  def patternPairIsolation(board: HBoard, item: HItem): HBoard =
  {

    var b = board

    val row = board.items.filter(_.y == item.y)
    val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    val col = board.items.filter(_.x == item.x)
    val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    conflictsRow.foreach(i =>
    {
      if (isNextToDuplicate(item, i) != "F")
      {
        conflictsRow.filter(j => j.x != item.x && j.y != item.y).filter(j => j.x != i.x && j.y != i.y).foreach(j => b = setCellBlack(b, j.x, j.y))
      }

    })

    conflictsCol.foreach(i =>
    {
      if (isNextToDuplicate(item, i) != "F")
      {
        conflictsCol.filter(j => j.x != item.x && j.y != item.y).filter(j => j.x != i.x && j.y != i.y).foreach(j => b = setCellBlack(b, j.x, j.y))
      }
    })

    return b
  }

  /** This section contains patterns that is used in phase 1 to solve different situations.
    * Every patterns should at least take a board and a item as input.
    * All patterns needs to return a new board (Altered or not)
    *
    * All patterns functions should be named after the following convention: patternSomeName
    */

  def patternDoubleCorner(board: HBoard, itemA: HItem, itemB: HItem): HBoard =
  {
    val f = itemA match
    {
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

  /**
    * This pattern checks if the board is 1x1.
    * @param board The board it checks
    * @return a solved board with white cell.
    */
  def patternOneTimesOne(board: HBoard): HBoard =
  {
    if(board.size == 0) return setCellWhite(board, 0, 0)
    return board
  }

  /**
    * This pattern finds triple corners (three similar values) and sets the parent cell as black
    * It compares a corner cell with neighbour cells
    * @param board The board it checks
    * @param itemA A item to check for the TripleCorner pattern
    * @param itemB
    * @return
    */
  def patternTripleCorner(board: HBoard, itemA: HItem, itemB: HItem): HBoard =
  {
    /*
    1. Sjekk om det er et hjørne
    2. Finn naboer
    3. Sjekk naboers value er lik itemA value
    4. Hvis ja, sett itemA som svart
     */

    val f = itemA match
    {
      case i if isInCorner(board, i) == "TL"
        && i.value == getCellXY(board, i.x + 1, i.y).value
        && i.value == getCellXY(board, i.x, i.y + 1).value => setCellBlack(board, i.x, i.y)
      case i if isInCorner(board, i) == "BL"
        && i.value == getCellXY(board, i.x + 1, i.y).value
        && i.value == getCellXY(board, i.x, i.y - 1).value => setCellBlack(board, i.x, i.y)
      case i if isInCorner(board, i) == "TR"
        && i.value == getCellXY(board, i.x - 1, i.y).value
        && i.value == getCellXY(board, i.x, i.y + 1).value => setCellBlack(board, i.x, i.y)
      case i if isInCorner(board, i) == "BR"
        && i.value == getCellXY(board, i.x - 1, i.y).value
        && i.value == getCellXY(board, i.x, i.y - 1).value => setCellBlack(board, i.x, i.y)

      case _ => board
    }

    return f
  }

  def patternFlippedTripleCorner(board: HBoard, itemA: HItem): HBoard =
  {

    val f = itemA match
    {
      case i if isInCorner(board, i) == "TL"
        && getCellXY(board, i.x + 1, i.y).value == getCellXY(board, i.x + 1, i.y + 1).value
        && getCellXY(board, i.x, i.y + 1).value == getCellXY(board, i.x + 1, i.y + 1).value => setCellBlack(board, i.x + 1, i.y + 1)
      case i if isInCorner(board, i) == "BL"
        && getCellXY(board, i.x + 1, i.y).value == getCellXY(board, i.x + 1, i.y - 1).value
        && getCellXY(board, i.x, i.y - 1).value == getCellXY(board, i.x + 1, i.y - 1).value => setCellBlack(board, i.x + 1, i.y - 1)
      case i if isInCorner(board, i) == "TR"
        && getCellXY(board, i.x - 1, i.y).value == getCellXY(board, i.x - 1, i.y + 1).value
        && getCellXY(board, i.x, i.y + 1).value == getCellXY(board, i.x - 1, i.y + 1).value => setCellBlack(board, i.x - 1, i.y + 1)
      case i if isInCorner(board, i) == "BR"
        && getCellXY(board, i.x - 1, i.y).value == getCellXY(board, i.x - 1, i.y - 1).value
        && getCellXY(board, i.x, i.y - 1).value == getCellXY(board, i.x - 1, i.y - 1).value => setCellBlack(board, i.x - 1, i.y - 1)

      case _ => board
    }

    return f
  }

  /**
    * This pattern finds quad corners (four similar values) and sets the parent cell and closest diagonal cell black.
    */
  def patternQuadCorner(board: HBoard, itemA: HItem, itemB: HItem): HBoard =
  {
    val f = itemA match
    {
      case i if isInCorner(board, i) == "TL"
        && i.value == getCellXY(board, i.x + 1, i.y).value //
        && i.value == getCellXY(board, i.x, i.y + 1).value
        && i.value == getCellXY(board, i.x + 1, i.y + 1).value => setCellBlack(board, i.x, i.y); setCellBlack(board, i.x + 1, i.y + 1)
      case i if isInCorner(board, i) == "BL"
        && i.value == getCellXY(board, i.x + 1, i.y).value
        && i.value == getCellXY(board, i.x, i.y - 1).value
        && i.value == getCellXY(board, i.x + 1, i.y - 1).value => setCellBlack(board, i.x, i.y); setCellBlack(board, i.x + 1, i.y - 1)
      case i if isInCorner(board, i) == "TR"
        && i.value == getCellXY(board, i.x - 1, i.y).value
        && i.value == getCellXY(board, i.x, i.y + 1).value
        && i.value == getCellXY(board, i.x - 1, i.y + 1).value => setCellBlack(board, i.x, i.y); setCellBlack(board, i.x - 1, i.y + 1)
      case i if isInCorner(board, i) == "BR"
        && i.value == getCellXY(board, i.x - 1, i.y).value
        && i.value == getCellXY(board, i.x, i.y - 1).value
        && i.value == getCellXY(board, i.x - 1, i.y - 1).value => setCellBlack(board, i.x, i.y); setCellBlack(board, i.x - 1, i.y - 1)
      case _ => board
    }

    return f
  }

  def patternSandwich(board: HBoard, itemA: HItem, itemB: HItem): HBoard =
  {
    itemA match
    {
      case i if itemB.x == i.x + 2 => setCellWhite(board, i.x + 1, i.y)
      case i if itemB.x == i.x - 2 => setCellWhite(board, i.x - 1, i.y)
      case i if itemB.y == i.y + 2 => setCellWhite(board, i.x, i.y + 1)
      case i if itemB.y == i.y - 2 => setCellWhite(board, i.x, i.y - 1)
      case _ => board
    }

  }


  /**
    * Pattern that determines it an item can be solved using the triple rule
    * If three items in a row or column have the same value, the item in the middle must be white
    *
    * @param board A Hitori board
    * @param item  A item to check for the triple rule
    * @return A new hitori board with possible changes
    */
  def patternTriple(board: HBoard, item: HItem): HBoard =
  {

    val b = board

    if (item.state == "W") return board
    val neighbours = getAllNeighbours(b, item)
    val neighboursRow = neighbours.filter(i => i != null).filter(i => i.y == item.y).filter(i => i.value == item.value)
    val neighboursCol = neighbours.filter(i => i != null).filter(i => i.x == item.x).filter(i => i.value == item.value)

    if (neighboursRow.length == 2 || neighboursCol.length == 2)
    {
      return setCellWhite(b, item.x, item.y)
    }


    return b
  }

  /* Phase-2 functions */

  /**
    * Phase 2 takes all of the items that has the state 'U' and tries to paint them black
    * If an item is painted black and an inconsistency occurs, we know it must be white.
    * If an item is paintent black and no inconsistency orrycs, we know nothing abouts its future state
    *
    * After phase2 is completed, it checks if any of the unsolved items are uniqi in their row.
    *
    * IMPORTANT!
    * Phase 2 will always return a valid board
    *
    * @param board A hitori board
    * @return A new hitori board with possible valid modifications
    */
  def phase2(board: HBoard): HBoard =
  {
    var b = board
    b = phase2Search(b)

    b.items.foreach(i => b = checkIfItemWillBeBlocked(b, i))
    b.items.foreach(i => b = checkForUniqueness(b, i))

    return b
  }


  def phase2Search(board: HBoard): HBoard =
  {

    val b = board
    var field = board

    val dup = unsolvedDuplicates(b)

    for (i <- dup)
    {
      field = setCellBlack(field, i.x, i.y)
      field = standardCycle(field, i)

      val noDuplicates = rule1(field)

      if (isConsistent(field))
        return field

      if (!noDuplicates)
      {
        field = b
        field = setCellWhite(field, i.x, i.y)
        field = standardCycle(field, i)
        return phase2Search(field)
      }
      else
      {
        field = b
      }
    }

    return field
  }


  def standardCycle(board: HBoard, item: HItem): HBoard =
  {

    var b = board
    // 3
    b.items.foreach(i => b = checkIfItemWillBeBlocked(b, i))

    return b
  }

  /**
    * Checks if a item is surounded with three black items (or edges)
    * If surrounded, the remaining unknown item must be white.
    *
    * If surrounded, the unknown cell is set as white
    *
    * @param board The board containing the item
    * @param itemA The item to check for black items
    * @return The board, altered if unknown cell is set to white.
    */
  def checkIfItemWillBeBlocked(board: HBoard, itemA: HItem): HBoard =
  {

    if (itemA.state == "B") return board;

    val neighbours = getAllNeighbours(board, itemA)

    var conflicts = neighbours.filter(i => i != null).filter(i => i.state == "B")

    neighbours.foreach(i => if (i == null) conflicts = conflicts ::: List(new HItem("F", -1, -1, "B")))

    if (conflicts.length < 3) return board

    val nonConflict = neighbours.filter(i => i != null).filter(i => i.state == "U");

    if (nonConflict.isEmpty) return board

    return setCellWhite(board, nonConflict.head.x, nonConflict.head.y)

  }

  /**
    * Checks if there exist any duplicates of the given item in its row or column
    * If there are no errors, the item is set to white.
    *
    * Ignores blacked cells
    *
    * @param board The board to scan
    * @param item  The item to compare to
    * @return A new board with changes only if the item was unique in its row/col
    */
  def checkForUniqueness(board: HBoard, item: HItem): HBoard =
  {

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

  /**
    * Hitori rule 1
    * " Numbers must not appear more than once in each row and each column."
    *
    * Checks if there exist any duplicates where the item's state is set to 'W'.
    * If there are any duplicates, the board will be in a inconistent state
    *
    * @param board The board to check for duplicates in
    * @return True if no duplicates, false if there are
    */
  def rule1(board: HBoard): Boolean =
  {
    board.items.foreach(item =>
    {
      if (item.state == "W")
      {
        val conflictsRow = board.items.filter(i => i.value == item.value).filter(i => i.y == item.y && i.x != item.x).filter(i => i.state == "W")
        val conflictsCol = board.items.filter(i => i.value == item.value).filter(i => i.x == item.x && i.y != item.y).filter(i => i.state == "W")

        if (conflictsRow.nonEmpty || conflictsCol.nonEmpty)
          return false

      }

    })

    return true
  }

  /**
    * Hitori rule 2
    * "Black cells are never adjacent in a row or column."
    *
    * Checks if there are two black cells adjacent to each other
    *
    * @param board The board to check for inconistensies
    * @return True if no errors, false if errors
    */
  def rule2(board: HBoard): Boolean =
  {

    board.items.foreach(item =>
    {

      if (item.state == "B")
      {
        val blackNeighbours = getAllNeighbours(board,item).filter(i => i != null).filter(i => i.state == "B")

        if(blackNeighbours.nonEmpty)
          return false
      }
    })

    return true
  }

  /**
    * Hitori rule 3
    * "Unpainted cells create a single continous area, undivided by painted cells"
    *
    * Checkis if all items are connected.
    *
    * @param board The board to check for inconistensies
    * @return True if no errors, false if errors
    */
  def rule3(board: HBoard): Boolean =
  {
    val traversableItemsFirst = board.items.filter(i => i.state != "B")
    var traversableItems = traversableItemsFirst

    var queue = Queue[HItem]()
    val visited = HashSet[HItem]()

    queue += traversableItems(0)

    while (!queue.isEmpty)
    {
      val item = queue.dequeue()

      visited.add(item)

      traversableItems = traversableItems.filter(i => i.x != item.x && i.y != item.y)

      val neighbours = getAllNeighbours(board, item).filter(i => i != null).filter(i => i.state != "B")

      neighbours.foreach(i => if (visited.add(i))
      {
        queue += i
      })
    }

    if (visited.size == traversableItemsFirst.length)
      return true

    return false
  }

  /**
    * Phase 3
    * MARK: With big boards, phase 3 takes time!
    *
    * Uses recursion to try and fail with different possible solutions.
    * Makes use of the standardCycle and phase2 for chain-reactions and checking
    *
    * @param board A partially solved board
    * @return A solved board.
    */
  def phase3(board: HBoard): HBoard =
  {

    var b = board
    var backup = b


    val dup = b.items.filter(i => i.state == "U")

    for (i <- dup)
    {
      b = setCellBlack(b, i.x, i.y)
      b = standardCycle(b, i)
      b = phase2(b)
      b = phase3(b)

      if (isConsistent(b))
        return b

      val rule_1 = rule1(b)
      val rule_3 = rule3(b)

      if (!rule_1 || !rule_3)
      {
        b = backup
        b = setCellWhite(b, i.x, i.y)
        backup = b
      } else
      {
        b = backup
      }

    }

    return b

  }


  /* General functions */

  def setDuplicatesBlack(board: HBoard, xPos: Int, yPos: Int): HBoard =
  {

    // The white cell
    val item = getCellXY(board, xPos, yPos);

    var b = board;

    // All elements in the white cells's row
    val row = b.items.filter(m => m.y == item.y);
    val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    for (i <- conflictsRow)
    {

      if (i.x != item.x)
      {
        b = setCellBlack(b, i.x, i.y);
      }
    }

    // All elements in the white cells's col
    val col = b.items.filter(m => m.x == item.x);
    val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).filter(_._1.state == "U").map(_._1)

    for (i <- conflictsCol.indices)
    {
      val itemToFix = conflictsCol(i);
      if (itemToFix.y != item.y)
      {
        b = setCellBlack(b, itemToFix.x, itemToFix.y);
      }
    }
    return b;
  }

  def setEdgesWhite(board: HBoard, xPos: Int, yPos: Int): HBoard =
  {
    val item = getCellXY(board, xPos, yPos);

    var b = board;

    b.items.foreach(m =>
    {
      m match
      {
        case i if (i.x == (item.x + 1) && i.y == item.y) => b = setCellWhite(b, i.x, i.y)
        case i if (i.x == (item.x - 1) && i.y == item.y) => b = setCellWhite(b, i.x, i.y)
        case i if (i.y == (item.y + 1) && i.x == item.x) => b = setCellWhite(b, i.x, i.y)
        case i if (i.y == (item.y - 1) && i.x == item.x) => b = setCellWhite(b, i.x, i.y)
        case _ => b = b
      }
    });

    return b;
  }

  def setCellBlack(board: HBoard, xPos: Int, yPos: Int): HBoard =
  {
    val item = getCellXY(board, xPos, yPos);

    if (item.state != "U") return board;

    val b = new HBoard(board.items.map(m => if (m.x == xPos && m.y == yPos) new HItem(item.value, item.x, item.y, "B") else m));
    return setEdgesWhite(b, xPos, yPos)
  }

  def setCellWhite(board: HBoard, xPos: Int, yPos: Int): HBoard =
  {
    val item = getCellXY(board, xPos, yPos);

    if (item.state != "U") return board;

    val b = new HBoard(board.items.map(m => if (m.x == xPos && m.y == yPos) new HItem(item.value, item.x, item.y, "W") else m));

    return setDuplicatesBlack(b, xPos, yPos);
  }

  def setCellBlackSimple(board: HBoard, item: HItem): HBoard =
  {

    if (item.state != "U") return board;

    return new HBoard(board.items.map(m => if (m.x == item.x && m.y == item.y) new HItem(item.value, item.x, item.y, "B") else m));
  }

  def setCellWhiteSimple(board: HBoard, item: HItem): HBoard =
  {
    if (item.state != "U") return board;

    return new HBoard(board.items.map(m => if (m.x == item.x && m.y == item.y) new HItem(item.value, item.x, item.y, "W") else m));
  }


  /* Helper functions */

  def duplicates(board: HBoard): List[HItem] =
  {


    var set: HashSet[HItem] = HashSet()

    for (index <- 0 to board.size)
    {

      val item = board.items.filter(i => i.x == index && i.y == index).head

      val row = board.items.filter(_.y == item.y);
      val conflictsRow: List[HItem] = row.zipWithIndex.filter(_._1.value == item.value).map(_._1)

      val col = board.items.filter(_.x == item.x);
      val conflictsCol: List[HItem] = col.zipWithIndex.filter(_._1.value == item.value).map(_._1)

      conflictsCol.foreach(i => set += i)
      conflictsRow.foreach(i => set += i)
    }

    return set.toList

  }

  def unsolvedDuplicates(board: HBoard): List[HItem] =
  {
    return duplicates(board).filter(i => i.state == "U")
  }

  def isNextToDuplicate(itemA: HItem, itemB: HItem): String =
  {
    itemA match
    {
      case i if (itemB.x == i.x + 1 && i.y == itemB.y) => "R" // ItemB is right of itemA
      case i if (itemB.x == i.x - 1 && i.y == itemB.y) => "L" // ItemB is left of itemA
      case i if (itemB.y == i.y + 1 && i.x == itemB.x) => "T" // ItemB is over itemA (Top)
      case i if (itemB.y == i.y - 1 && i.x == itemB.x) => "B" // ItemB is under itemA (Bottom)
      case _ => "F";
    }
  }

  def isInCorner(board: HBoard, itemA: HItem): String =
  {
    val bs = math.sqrt(board.items.length) - 1;
    //0 1 2 3 4
    itemA match
    {
      case i if i.x == 0 && i.y == 0 => "TL" // TopLeft
      case i if i.x == bs && i.y == 0 => "TR" // TopRight
      case i if i.x == 0 && i.y == bs => "BL" // BottomLeft
      case i if i.x == bs && i.y == bs => "BR" // BottomRight
      case _ => "F";
    }
  }

  def isInCornerPlusOne(board: HBoard, itemA: HItem): String =
  {
    val bs = math.sqrt(board.items.length) - 1;

    val i = itemA match
    {
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

  def getCellXY(board: HBoard, xPos: Int, yPos: Int): HItem =
  {


    if (xPos < 0 || yPos < 0 || xPos > board.size || yPos > board.size) return null

    return board.items.filter(i => i.x == xPos).filter(i => i.y == yPos).head
  }

  def printBoard(board: HBoard): Unit =
  {

    val b = new HBoard(board.items.sortWith(_.x < _.x).sortWith(_.y < _.y));

    println("------------------------------------------------")
    for (y <- 0 to board.size)
    {

      for (x <- 0 to board.size)
      {
        val item = getCellXY(b, x, y)
        print(item.state + " ")
      }

      print("\n")
    }

    println("------------------------------------------------")
  }

  def getAllNeighbours(board: HBoard, itemA: HItem): List[HItem] =
  {
    val left = getCellXY(board, itemA.x - 1, itemA.y)
    val right = getCellXY(board, itemA.x + 1, itemA.y)

    val up = getCellXY(board, itemA.x, itemA.y - 1)
    val down = getCellXY(board, itemA.x, itemA.y + 1)

    return List(left, right, up, down)
  }

  /**
    * Checks if the board is solved
    * Uses the three rules of Hitori and checks if all cells is in a black or white cell
    *
    * @param board The board to check for consistency
    * @return True if consistent, false if not
    */
  def isConsistent(board:HBoard):Boolean =
  {
    val noDuplicates = rule1(board)
    val noBlackNeighbours = rule2(board)
    val isConnected = rule3(board)
    val noUnsolvedItems = board.items.filter(m => m.state == "U").isEmpty;

    if(noDuplicates && noBlackNeighbours && isConnected && noUnsolvedItems)
      return true

    return false
  }

  /**
    * Checks if the board is not 1x1 or symmetrical.
    * @param board Takes in the board to check
    * @return Boolean whether the board is valid or not
    */
  def isBoardValid(board: HBoard): Boolean =
  {
    if ((board.items.size / (board.size + 1)) != board.size + 1) return false

    return true
  }

  /**
    *
    * @param board
    * @param outputPath
    */
  def saveSolvedPuzzle(board: HBoard, outputPath: String): Unit =
  {
    // Solve puzzle and output to file, like so:

    val outputFile = new PrintWriter(new File(outputPath), "UTF-8")
    val b = new HBoard(board.items.sortWith(_.x < _.x).sortWith(_.y < _.y));

    for (y <- 0 to board.size)
    {

      for (x <- 0 to board.size)
      {
        val item = getCellXY(b, x, y)
        outputFile.print(item.state + " ")
      }

      outputFile.print("\n")
    }

    outputFile.close()
  }

  /**
    * Load the file from the specified path
    *
    * @param inputPath The filepath of the board (Relative to the src folder)
    * @return The hitori-board parsed from the given file.
    */
  def loadGameFromFile(inputPath: String): HBoard =
  {
    val puzzleFile = new File(inputPath)
    val lines = scala.io.Source.fromFile(puzzleFile).mkString.split("\r\n");

    var board: HBoard = new HBoard(List())

    lines.indices.foreach(y =>
    {
      val row = lines(y).split(" ");
      row.indices.foreach(x => board = new HBoard(board.items ::: List(new HItem(row(x), x, y))))
    });

    // TODO: Remove this for better performance!
    // Prints the board (for testing and debugging)
    lines.foreach(i => println(i))

    return board

  }

}