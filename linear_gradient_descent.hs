{-# LANGUAGE ViewPatterns #-}
import Numeric.LinearAlgebra.HMatrix
import Text.CSV
import Data.Maybe (fromMaybe)
import Data.Either

-- Normalize an array 
normalize :: ( Fractional a)  => [a] -> a -> a -> [a]
normalize arr minval maxval = map (func) arr
    where 
        func x = (x - minval) / (maxval - minval)

--denormalize an already normalized arrays
denormalize :: ( Fractional a)  => [a] -> a -> a -> [a]
denormalize arr minval maxval = map (func) arr
    where 
        func x = x * (maxval-minval) + minval

getweights weights = print (weights)
     
--using batch gradient descent to minimize function (in this case mean squared error) for a linear function (mx + b)
linear_descent ::  Matrix Double -> Matrix Double -> Matrix Double ->  Double ->   Double ->  Double ->  (Matrix  Double, Double)
linear_descent x y weights offset error  iters 
    | iters > 10000 = (weights ,  offset)
    | error < error_goal &&  error  /=  0    =  (weights,  offset)
    | otherwise =    
        let 
            lr = 0.1 :: Double
            train_pred_mx= map sum (toLists  ( x * weights) )
            train_pred = map (+offset) train_pred_mx
            train_error =  (sumElements (y - (matrix 1 train_pred)) ^2 ) / realToFrac  (length train_pred)
            gradient_offset = - ( 2 / realToFrac (rows y ) ) * sumElements(y - (matrix 1 train_pred))
            gradient_weights = map ( * ( - ( 2 / realToFrac (rows y ) )))  (map sum (toLists(tr  (x   * (y - (matrix 1 train_pred))))))
            scaled_gradient_weights = map (*lr) gradient_weights
            new_offset = offset - lr * gradient_offset
            intermediate_weights = (rows weights >< cols weights) (take (rows weights * cols weights) (cycle scaled_gradient_weights)) :: Matrix Double
            new_weights = weights -  intermediate_weights
        in linear_descent x y new_weights new_offset train_error (iters + 1)
    where 
        error_goal = 0.01
    
main :: IO()
main =  do 
            dat <- parseCSVFromFile "/mnt/c/Users/Sir/Downloads/Wage.csv"
            let [rs]= rights [dat] 
            let extractRows x = [ record | record <- rs]
            let p = head(  map extractRows rs )
            let (keys:values) = p
            let convert_values arr = [read x :: Double | x <- arr]
            let value_nums = map convert_values (init values)
            let value_matrix = matrix (length keys)  (concat value_nums) :: Matrix Double
            let x = value_matrix ?? (All, Take ((length keys)-1))
            let to_normalize = (toLists (tr x))
            let normalize arr minval maxval = map (func) arr where func x = (x - minval) / (maxval - minval)
            let do_normalizing arr = normalize arr (minimum arr) (maximum arr)
            let normalized_values = map do_normalizing to_normalize
            let x= tr ( fromLists normalized_values ) :: Matrix Double

            let y1 =  value_matrix ?? (All, TakeLast 1)
            --let y1 = matrix 1 (do_normalizing (toLists (tr y) !! 0) ) :: Matrix Double
            let  avg x = sum  x / realToFrac (length x)
            let weights = ((rows x) >< (cols x)) (take (rows x * cols x) (cycle (map avg (toLists (tr x)))))  :: Matrix Double
            let offset = 1
            print (keys)
            let (w, o) = linear_descent x y1 weights offset 0.5 0
            print ("results", (w ?? (Take 1, All), o))
