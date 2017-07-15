# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + 
                [Piece.rotations([[0, 0], [-1, 0], [0, -1], [-1, -1], [1, -1]]),
                 [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], 
                  [[0, 0], [0, 1], [0, 2], [0, -1], [0, -2]]],
                  Piece.rotations([[0, 0], [0, -1], [1, 0]])]
  Cheated_Pieces = [[[0, 0]]]
  # your enhancements here
  def self.next_piece (board)
    if(board.cheated)
      MyPiece.new(Cheated_Pieces, board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end
end

class MyBoard < Board
  # your enhancements here
  Cost = 100
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
  end

  def cheated
    @cheated
  end


  def rotate180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    if !game_over? and @game.is_running?
      if @score >= Cost and !@cheated
        @score -= Cost
        @game.update_score
        @cheated = true
      end
    end
  end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
    @cheated = false
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate180})
    @root.bind('c', proc {@board.cheat})
  end
end


