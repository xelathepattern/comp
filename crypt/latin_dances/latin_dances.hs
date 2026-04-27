import Data.Word (Word32)
import Data.Bits (rotateL, xor)

data Block = Block 
    { w0 :: Word32
    , w1 :: Word32
    , w2 :: Word32
    , w3 :: Word32
    , w4 :: Word32
    , w5 :: Word32
    , w6 :: Word32
    , w7 :: Word32
    , w8 :: Word32
    , w9 :: Word32
    , w10 :: Word32
    , w11 :: Word32
    , w12 :: Word32
    , w13 :: Word32
    , w14 :: Word32
    , w15 :: Word32
    } deriving Show

le :: Int -> Int -> Int -> Int -> Word32
le w x y z = fromIntegral (w + 2^8 * x + 2^16 * y + 2^24 * z) :: Word32

blockSum :: Block -> Block -> Block
blockSum (Block a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) (Block o0 o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15) = Block (a0 + o0) (a1 + o1) (a2 + o2) (a3 + o3) (a4 + o4) (a5 + o5) (a6 + o6) (a7 + o7) (a8 + o8) (a9 + o9) (a10 + o10) (a11 + o11) (a12 + o12) (a13 + o13) (a14 + o14) (a15 + o15)


qr :: Word32 -> Word32 -> Word32 -> Word32 -> (Word32, Word32, Word32, Word32)
qr w x y z = (a, b, c, d)
  -- y_i ^= (y_((i-1)%4) + y_((i+2)%4)) << magic_i
  -- i starts at 1 ends at 4=0
  where b = x `xor` rotateL (w + z) 7
        c = y `xor` rotateL (b + w) 9
        d = z `xor` rotateL (c + b) 13
        a = w `xor` rotateL (d + c) 18

rowr :: Block -> Block
rowr (Block x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = Block a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
  -- ith rows words is shifted left i times on the word level, sent to qr, then output is shifted right i times on the word level
  where (a0, a1, a2, a3) = qr x0 x1 x2 x3
        (a5, a6, a7, a4) = qr x5 x6 x7 x4
        (a10, a11, a8, a9) = qr x10 x11 x8 x9
        (a15, a12, a13, a14) = qr x15 x12 x13 x14

colr :: Block -> Block
colr (Block x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = Block a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
  -- ith rows words is shifted left i times on the word level, sent to qr, then output is shifted right i times on the word level
  where (a0, a4, a8, a12) = qr x0 x4 x8 x12
        (a5, a9, a13, a1) = qr x5 x9 x13 x1
        (a10, a14, a2, a6) = qr x10 x14 x2 x6
        (a15, a3, a7, a11) = qr x15 x3 x7 x11


dr = rowr . colr

salsa20 x@(Block x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = blockSum x o
  where rounds = 10 
        o =  iterate dr x !! rounds
        
