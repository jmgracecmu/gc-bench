structure Color =
struct
  type channel = Word8.word
  type pixel = {red: channel, green: channel, blue: channel}

  val white: pixel = {red=0w255, green=0w255, blue=0w255}
  val black: pixel = {red=0w0, green=0w0, blue=0w0}
  val red: pixel = {red=0w255, green=0w0, blue=0w0}
end
