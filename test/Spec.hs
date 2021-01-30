import Internals.VirtualFileOffset
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Bits
import Data.Word

isWord n x =  x >= 0 && x < 2^n
correctWords x y = isWord 48 x && isWord 16 y
correctProp x y =  let y' = shiftR y 48
                       x' = shiftR x 16
                   in virtualOffset ((shiftL x' 16) .|.  y') == VirtualFileOffset (fromIntegral x') (fromIntegral y') 
                     where types=[x::Word64, y::Word64]


main :: IO ()
main = hspec $ do
       describe "Virtual Offset" $ do
         it "Works with zero" $ do
           virtualOffset 0 `shouldBe` VirtualFileOffset 0 0

         it  "Works with 1" $ do
           virtualOffset 1 `shouldBe` VirtualFileOffset 0 1
         
         prop "Works in general" $ 
            correctProp
           
  
