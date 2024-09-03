//Вариант 21
let e =  2.7182

// Series 1 + 2(x/2)+ ... + ((n**2 + 1)/ !n)((x/2) ** n)
// 
// function (x ** 2/4 + x/ 2 + 1) * (e **(x / 2))

let f x = ((x ** 2. / 4.) + (x/ 2.) + 1.) * (e **(x / 2.))
let a = 0.1
let b = 0.6
let steps = 10
let eps = 0.001

    
let rec fac = function
    | 0. -> 1.
    | 1. -> 1.
    | n -> n*fac(n - 1.)

let rec myfor f i a b =
    if a<=b then 
        f (myfor f i (a + 1.) b) a 
    else i

let pow a n = 
    myfor (fun acc _ -> acc * a) 1. 1. n

let absDiff a b =
    if a > b then a-b
    else b-a

let dumbTaylor x eps = 
    let rec dumbTaylor' n sum x eps = 
      let cur = (((pow n 2.) + 1.) / (fac n)) * (pow (x / 2.) n)
      //printf "%f " sum
      if (cur < eps) then
        (sum,n)
      else
        let sum = sum + cur
        dumbTaylor' (n + 1.) sum x eps
    dumbTaylor' 0. 0. x eps

let smartTailor x  eps=
    let rec smartTailor' x n prev sum eps =
        let cur = prev * (x / 2.) * (((pow n 2.) + 1.) / (n * ((pow (n - 1.) 2.) + 1.)))
        //printf "%f" cur
        if cur < eps then 
            (sum, n)
        else 
            let sum = sum + cur
            smartTailor' x (n+1.)  cur sum eps
    smartTailor' x 1. 1. 1. eps



let main =
    printfn " x \t\t\t\t f(x) \t\t dumbTaylor\t n \t smartTailor \t n"
    for j=0 to steps do
        let x = a+(float j)/(float steps)*(b-a)
        printf "%f" x
        let naive_t = dumbTaylor x eps
        let smart_t = smartTailor x eps
        printfn "%5.2f  %10.6f   %10.6f   %3.1f   %10.6f  %3.1f" x (f x) (fst naive_t) (snd naive_t) (fst smart_t) (snd smart_t)

main