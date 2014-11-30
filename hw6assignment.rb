# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  def initialize(point_array, board)
      super(point_array, board)
  end

  def self.next_piece(board)
      if board.set_check
          board.set_check = false
          piece = [[[0,0]]]
      else
          piece = All_Pieces.sample
      end
      MyPiece.new(piece, board)
  end

  # your enhancements here
  #
 
  All_My_Pieces = All_Pieces

  All_Pieces << [ [[0,0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
                    [[0,0], [0, 1], [0, 2], [0, -1], [0, -2]]]
  All_Pieces << rotations([[0, 0], [0, 1], [1,1]])
  All_Pieces << rotations([[0,0], [-1,0], [-1,1], [0, 1], [1, 1]])

end

class MyBoard < Board
  # your enhancements here
  attr_accessor :set_check

  def initialize(game)
      super(game)
      @set_check = false
  end

  def rotate180
      if !game_over? and @game.is_running?
          @current_block.move(0, 0, 1)
          @current_block.move(0, 0, 1)
      end
      draw
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    locations.each_index{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def check
    return if @set_check or score < 100
    @set_check = true
    @score -= 100
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

end

class MyTetris < Tetris
  # your enhancements here

  def initialize
      super
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end


  def check
      @board.check
      update_score
  end

  def key_bindings
      super

      @root.bind('c', proc {self.check})
      @root.bind('u', proc {@board.rotate180})
  end
end
