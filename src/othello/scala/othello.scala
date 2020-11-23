/*
プログラムの実行手順：
1. ターミナル / コマンドプロンプトを開く
2. build.sbt が置かれた場所で sbt と入力し、return を押す
3. project othello と入力し、return を押す
4. run と入力し、return を押す
5. プログラムを変更後、もう一度実行したいときは run と入力し、return を押す
*/

package othello

import scala.math.max
import scala.math.min

object OthelloLib {

  // マス目
  trait Square

  // 空のマス目
  case object Empty extends Square

  // プレイヤー
  trait Player extends Square

  // 黒・白のプレイヤー
  case object Black extends Player
  case object White extends Player

  // xy 座標の点
  type Position = (Int, Int)

  // ゲームの盤面
  type Board = List[List[Square]]

  // 盤面の初期値
  val initBoard: Board =
    List(List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, White, Black, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Black, White, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty))

  // マス目の座標のリスト
  val posList: List[Position] =
    List((1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1), (8, 1),
      (1, 2), (2, 2), (3, 2), (4, 2), (5, 2), (6, 2), (7, 2), (8, 2),
      (1, 3), (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3),
      (1, 4), (2, 4), (3, 4), (4, 4), (5, 4), (6, 4), (7, 4), (8, 4),
      (1, 5), (2, 5), (3, 5), (4, 5), (5, 5), (6, 5), (7, 5), (8, 5),
      (1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (7, 6), (8, 6),
      (1, 7), (2, 7), (3, 7), (4, 7), (5, 7), (6, 7), (7, 7), (8, 7),
      (1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (6, 8), (7, 8), (8, 8))

  // ゲームの状態
  type Game = (Board, Player)

  // ゲームの初期値
  val newGame = (initBoard, Black)

  // board の pos にあるマス目の情報を返す
  def boardRef(board: Board, pos: Position): Square = {
    val (x, y) = pos
    board(y - 1)(x - 1)
  }

  // board の pos に player が石を置いたときに、相手の石を取り囲むかを判定する
  def outflanks(board: Board, player: Player, pos: Position): Boolean = {
    val (x, y) = pos
    (boardRef(board, pos) == Empty) && (!(flippedPositions(board, player, pos) == Nil))
  }

  // player の敵を返す
  def opponent(player: Player): Player = {
    if (player == Black) White else Black
  }

  // board 上の player の石の数を返す
  def countPieces(game: Game): Int = {
    val (board, player) = game
    board.foldRight(0)((row, r) => row.foldRight(0)((s, r) => if (s == player) r + 1 else r) + r)
  }

  // マス目の中身を文字列に変換する
  def squareToString(s: Square): String = {
    s match {
      case Empty => "."
      case Black => "x"
      case White => "o"
    }
  }

  // 盤面の描画
  def drawBoard(board: Board): Unit = {

    def drawRow(row: List[Square]): Unit = {
      row match {
        case Nil     => printf("\n")
        case s :: ss => printf("%s ", squareToString(s)); drawRow(ss)
      }
    }

    board match {
      case Nil     => printf("\n"); printf("---------------\n")
      case r :: rs => drawRow(r); drawBoard(rs)
    }
  }

  // ゲームオーバかどうかを判定する
  def gameOver(game: Game): Boolean = {
    val (board, player) = game
    !(posList.foldRight(false)((p, b) => b || outflanks(board, player, p)) ||
      posList.foldRight(false)((p, b) => b || outflanks(board, opponent(player), p)))
  }

  // board の pos に player が石を置いたときに、色が反転するマス目の座標を返す
  def flippedPositions(board: Board, player: Player, pos: Position): List[Position] = {

    // Position の足し算
    def posPlus(pos: Position, x: Int, y: Int): Position = {
      val (x0, y0) = pos
      (x0 + x, y0 + y)
    }

    // pos から (x, y) 方向に向かって反転する点を探す
    def trim(pos: Position, x: Int, y: Int): List[Position] = {

      def trimAux(pos: Position, x: Int, y: Int, list: List[Position]): List[Position] = {
        val nextPos = posPlus(pos, x, y)
        nextPos match {
          case (nextx, nexty) =>
            if ((nextx < 1) || (nextx > 8) || (nexty < 1) || (nexty > 8)) Nil
            else if (boardRef(board, nextPos) == Empty) Nil
            else if (boardRef(board, nextPos) == player) list
            else trimAux(nextPos, x, y, nextPos :: list)
        }
      }

      trimAux(pos, x, y, Nil)
    }

    trim(pos, 0, 1) ++ trim(pos, 1, 1) ++ trim(pos, 1, 0) ++ trim(pos, 1, -1) ++
    trim(pos, 0, -1) ++ trim(pos, -1, -1) ++ trim(pos, -1, 0) ++ trim(pos, -1, 1)
  }

  // player が石を置ける board 上の座標のリストを返す
  def validMoves(board: Board, player: Player): List[Position] = {
    posList.filter(outflanks(board, player, _)).filter(boardRef(board, _) == Empty)
  }

  // board の pos に player が石を置いた結果、得られる状態を返す
  def applyMove(board: Board, player: Player, pos: Position): Game = {

    // 行数の分だけ makeRow を行い、その結果を使って盤面を作る
    def makeBoard(board: Board, flipList: List[Position], y: Int): Board = {
      board match {
        case Nil => Nil
        case r :: rs =>
          // y 行目の座標のうち、色が反転するもののリスト
          val flipListY = flipList.filter(p => p match { case (px, py) => py == y })
          makeRow(r, flipListY, 1, y) :: makeBoard(rs, flipList, y + 1)
      }
    }

    // 反転後の行を作る
    def makeRow(row: List[Square], flipListY: List[Position], x: Int, y: Int): List[Square] = {
        row match {
          case Nil => Nil
          case square :: squares => {
            val newSquare =
              // 反転リストに入っている座標は player
              if (contain(x, flipListY)) player
              // player が石を置く場所は player
              else if (pos == (x, y)) player
              // それ以外はそのまま
              else square
            newSquare :: makeRow(squares, flipListY, x + 1, y)
          }
        }
    }

    // (x, y) が flipListY に含まれるかを判定
    def contain(x: Int, flipListY: List[Position]): Boolean = {
      flipListY match {
        case Nil => false
        case (px, py) :: ps => if (px == x) true else contain(x, ps)
      }
    }

    if (!(outflanks(board, player, pos))) {
      throw new Exception("not a valid move")
    }
    else {
      // 反転する座標のリスト
      val flipList = flippedPositions(board, player, pos)
      // 反転後の盤面
      val newboard = makeBoard(board, flipList, 1)
      // 次のプレイヤー
      val nextplayer = opponent(player)
      (newboard, nextplayer)
    }
  }

  // 戦略
  type Strategy = Game => Position

  // posList を前から順番に見ていき、可能な手を見つける
  def firstMove(game: Game): Position = {

    def firstMoveAux(list: List[Position]): Position = {
      val (board, player) = game
      list match {
        case Nil => throw new Exception("no valid move")
        case p :: ps => if (outflanks(board, player, p)) p else firstMoveAux(ps)
      }
    }

    firstMoveAux(posList)
  }

  // 人間のキー入力を受け取る
  def human: Strategy = {
    game =>
      val (board, player) = game
      val strMove = io.StdIn.readLine().split(' ')
      val move = (strMove(0).toInt, strMove(1).toInt)
      if (!(outflanks(board, player, move))) {
        println("Not a valid move! Please try again.");
        human(game)
      }
      else move
  }

  // ヒューリスティック
  type Heuristic = Game => Int

  // 黒 - 白 の値を返す
  def countDiff: Heuristic = {
    game => countPieces(game._1, Black) - countPieces(game._1, White)
  }

  // 戦略の適用
  def applyStrategy(game: Game, strategy: Strategy): Game = {
    val (board, player) = game
    val nextPlayer = opponent(player)
    if (!(posList.foldRight(false)((p, b) => b || outflanks(board, player, p)))) {
       printf("skip!\n");
       (board, nextPlayer)
    }
    else {
      val pos = strategy(game)
      if (!(outflanks(board, player, pos))) {
        throw new Exception("invalid move")
      }
      else {
        applyMove(board, player, pos)
      }
    }
  }

  // ゲームの開始
  def playLoop(game: Game, strat1: Strategy, strat2: Strategy): Game = {
    val (board, player) = game
    if (gameOver(game)) {
      val blackscore = countPieces(board, Black)
      val whitescore = countPieces(board, White)
      val winner =
        if (blackscore > whitescore) "Black"
        else if (whitescore > blackscore) "White"
        else "None"
      drawBoard(board);
      printf("Black: %d, White: %d, Winner: %s\n", blackscore, whitescore, winner);
      sys.exit()
    }
    else {
      drawBoard(board);
      val newgame = applyStrategy(game, strat1)
      playLoop(newgame, strat2, strat1)
    }
  }

  /////////
  // 課題 //
  /////////

  // 1. minimaxEval
  // 目的：minimax 法に基づいてゲームの状態を評価する
  def minimaxEval(heuristic: Heuristic, depth: Int, game: Game): Int = {
    val (board,player) = game
    if(gameOver(game)) return countDiff(game)
    if(depth==0) return heuristic(game)
    if(validMoves(board,player)==Nil) return minimaxEval(heuristic,depth,(board,opponent(player)))

    val valuelist = validMoves(board,player).map(applyMove(board,player,_)).map(minimaxEval(heuristic,depth-1,_))
    player match{
      case Black => valuelist.max
      case White => valuelist.min
    }
  }

  // 2. minimax
  // 目的：minimax 法に基づいて最適な手を求める関数
  def minimax(heuristic: Heuristic, depth: Int): Strategy = {
    game =>
      val (board,player) = game
      def addvalue(heuristic:Heuristic,depth:Int,game:Game,pos:Position):(Position,Int) = {
        val (b,p) = game
        (pos,minimaxEval(heuristic,depth-1,applyMove(b,p,pos)))
      }
      val nextlist = validMoves(board,player).map(addvalue(heuristic,depth,game,_))
      player match{
        case Black => nextlist.foldLeft(((0, 0), Int.MinValue))((x, pv) => if (x._2<pv._2) pv else x)._1
        case White => nextlist.foldLeft(((0, 0), Int.MaxValue))((x, pv) => if (x._2>pv._2) pv else x)._1
      }
  }

  // 3. alphabetaEval
  // 目的：alpha-beta 法に基づいてゲームの状態を評価する
  def alphabetaEval(heuristic: Heuristic, depth: Int, a: Int, b: Int, game: Game): Int = {
    val (board,player) = game
    if(gameOver(game)) return countDiff(game)
    if(depth==0) return heuristic(game)
    if(validMoves(board,player)==Nil) return minimaxEval(heuristic,depth,(board,opponent(player)))

    player match{
      case Black => 
        var v = Int.MinValue
        var A = a
        for(x <- validMoves(board,player)){
          v = max(v,alphabetaEval(heuristic,depth-1,A,b,applyMove(board,player,x)))
          A = max(A,v)
          if(A >= b) return v
        }
        return v
        

      case White => 
        var v = Int.MaxValue
        var B = b
        for(x <- validMoves(board,player)){
          v = min(v,alphabetaEval(heuristic,depth-1,a,B,applyMove(board,player,x)))
          B = min(B,v)
          if(a >= B) return v
        }
        return v

    }
  }

  // 4. alphabeta
  // 目的：alpha-beta 法に基づいて最適な手を求める
  def alphabeta(heuristic: Heuristic, depth: Int): Strategy = {
    game =>
      val (board,player) = game
      def addvalue2(heuristic:Heuristic,depth:Int,a:Int,be:Int,game:Game,pos:Position):(Position,Int) = {
        val (b,p) = game
        (pos,alphabetaEval(heuristic,depth-1,a,be,applyMove(b,p,pos)))
      }
      val nextlist = validMoves(board,player).map(addvalue2(heuristic,depth,Int.MinValue,Int.MaxValue,game,_))
      player match{
        case Black => nextlist.foldLeft(((0, 0), Int.MinValue))((x, pv) => if (x._2<pv._2) pv else x)._1
        case White => nextlist.foldLeft(((0, 0), Int.MaxValue))((x, pv) => if (x._2>pv._2) pv else x)._1
      }
  }
}

object OthelloMain extends App {
  import OthelloLib._

  // どれか1つのコメントを外す

  // 黒, 白ともに firstMove
  // playLoop(newGame, firstMove, firstMove)

  // 黒：人間, 白：firstMove
  // playLoop(newGame, human, firstMove)

  // 黒, 白ともに深さ4の minimax 法
  // playLoop(newGame, minimax(countDiff, 5), minimax(countDiff, 5))

  // 黒, 白ともに深さ4の alpha-beta 法
   playLoop(newGame, alphabeta(countDiff, 8), alphabeta(countDiff, 8))
}

// 5. 実験結果
/*
実験１
黒の戦略：minimax,5
白の戦略：minimax,5
黒 vs. 白の数：38 : 26
実行時間 (Total time)：24s

実験２
黒の戦略：alpha-beta,5
白の戦略：alpha-beta,5
黒 vs. 白の数：38 : 26
実行時間 (Total time)：6s

実験３
黒の戦略：alpha-beta,3
白の戦略：alpha-beta,4
黒 vs. 白の数：0 : 58
実行時間 (Total time)：3s

実験４
黒の戦略：alpha-beta,4
白の戦略：alpha-beta,5
黒 vs. 白の数：25 : 39
実行時間 (Total time)：7s

実験５
黒の戦略：alpha-beta,5
白の戦略：alpha-beta,6
黒 vs. 白の数：15 : 49
実行時間 (Total time)：12s

実験６
黒の戦略：alpha-beta,4
白の戦略：alpha-beta,4
黒 vs. 白の数：36 : 28
実行時間 (Total time)：3s

実験７
黒の戦略：alpha-beta,6
白の戦略：alpha-beta,6
黒 vs. 白の数：52 : 12
実行時間 (Total time)：11s

実験８
黒の戦略：alpha-beta,7
白の戦略：alpha-beta,7
黒 vs. 白の数：14 : 50
実行時間 (Total time)：96s

実験９
黒の戦略：alpha-beta,8
白の戦略：alpha-beta,8
黒 vs. 白の数：63 : 0
実行時間 (Total time)：128s

考察：実験１、２から、枝刈りの有無は結果には影響しない。実行時間は圧倒的にalpha-betaの方が短い。探索度を変えて行うと、１でも大きい方が全ての実験（３〜５）で勝ったので、探索度が正義。多少先行の方が有利にあるのかもしれないが、実験８では白が勝った。


*/
