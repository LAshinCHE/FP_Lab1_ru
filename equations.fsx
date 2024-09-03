open System

let eps = 0.001

let f1 x = (tan x) - ((1./3.) * ((tan x) ** 3.)) + ((1./5.) * (tan x) ** 5) - 1./3.
let f2 x = (acos x) - System.Math.Sqrt(1. - 0.3 * (x ** 3.))
let f3 x = 3. * x - 4. * System.Math.Log(x) - 5.

let f1' x = ((tan x) ** 4. - (tan x) ** 2. + 1.) /  (cos(x) ** 2.)
let f2' x = - ((20.* (System.Math.Sqrt(10. - 3.*(x **3.))) - 9. * System.Math.Sqrt(10) * (x **2.) * System.Math.Sqrt(1. - (x ** 2.))) / (20.*System.Math.Sqrt(1.-(x**2.))*System.Math.Sqrt(10. - 3.*(x ** 3.))))
let f3' x = (3.*x - 4.) / x


let phi1 x = x - f1 x / f1' x
let phi2 x = x - f2 x / f2' x
let phi3 x = x - f3 x / f3' x


let a1 = 0.
let b1 = 0.8

let a2 = 0.
let b2 = 1.

let a3 = 2.
let b3 = 4.

let absDiff a b =
    if a > b then a-b
    else b-a

let rec dichotomy f l r =
   let mid = (l + r) / 2.0
   if absDiff l r < eps then  
         mid
   else if f l * f mid < 0.0 then 
         dichotomy f l mid
   else 
         dichotomy f mid r

let iterations phi x' = 
    let rec myloop x = 
        if absDiff (phi x) x < eps then 
            phi x
        else 
            myloop (phi x)
    myloop x'

    
let newthon f f' x' = 
    iterations (fun x -> x - ((f x) / (f' x))) x'

let main =
    printfn "a  b \t Dichotomy\t Iterations \t Newthon"
    printfn "%f %f  %10.5f  %10.5f  %10.5f" a1 b1 (dichotomy f1 a1 b1) (iterations phi1 ((a1 + b1) / 2.0)) (newthon f1 f1' ((a1 + b1) / 2.0))
    printfn "%f %f  %10.5f  %10.5f  %10.5f" a2 b2 (dichotomy f2 a2 b2) (iterations phi2 ((a2 + b2) / 2.0)) (newthon f2 f2' ((a2 + b2) / 2.0))
    printfn "%f %f  %10.5f  %10.5f  %10.5f" a3 b3 (dichotomy f3 a3 b3) (iterations phi3 ((a3 + b3) / 2.0)) (newthon f3 f3' ((a3 + b3) / 2.0))

main