open AOC.Misc
open MathNet.Numerics.LinearAlgebra

type Ray = { Origin: Vector<double>; Velocity: Vector<double> }

let parse = 
    let parseVector = splitOn ", " >> Array.map double >> List.ofArray >> vector
    let parseLine = splitOn " @ " >> tuple >> tmap parseVector >> fun (o, v) -> { Origin = o; Velocity = v }
    Seq.map parseLine

let intersection r1 r2 =
    let A = matrix [| 
        [ r1.Velocity[0]; -r2.Velocity[0] ]
        [ r1.Velocity[1]; -r2.Velocity[1] ]
    |]
    let b = (r2.Origin - r1.Origin)[0..1]
    let (t, u) = A.Solve(b) |> Vector.toArray |> tuple
    if (t >= 0.0 && u >= 0.0) then Some (r1.Origin + r1.Velocity * t) else None

let part1 lo hi rays = 
    Seq.allPairs rays rays
    |> Seq.filter (fun (r1, r2) -> r1 <> r2)
    |> Seq.choose (fun (r1, r2) -> intersection r1 r2)
    |> Seq.filter (fun v -> v[0] >= lo && v[0] <= hi && v[1] >= lo && v[1] <= hi)
    |> Seq.length
    |> flip (/) 2

let part2 rays = 
    let pick3 = Seq.toArray >> fun a -> (a[0], a[1], a[2])
    let (A, B, C) = rays |> pick3
    let (Ax, Ay, Az) = A.Origin |> Vector.toArray |> triple
    let (Vax, Vay, Vaz) = A.Velocity |> Vector.toArray |> triple
    let (Bx, By, Bz) = B.Origin |> Vector.toArray |> triple
    let (Vbx, Vby, Vbz) = B.Velocity |> Vector.toArray |> triple
    let (Cx, Cy, Cz) = C.Origin |> Vector.toArray |> triple
    let (Vcx, Vcy, Vcz) = C.Velocity |> Vector.toArray |> triple
    let M = matrix [|
        [ Vay - Vby; Vbx - Vax; 0;         By - Ay; Ax - Bx; 0       ]
        [ Vay - Vcy; Vcx - Vax; 0;         Cy - Ay; Ax - Cx; 0       ]
        [ Vbz - Vaz; 0;         Vax - Vbx; Az - Bz; 0;       Bx - Ax ]
        [ Vcz - Vaz; 0;         Vax - Vcx; Az - Cz; 0;       Cx - Ax ]
        [ 0;         Vaz - Vbz; Vby - Vay; 0;       Bz - Az; Ay - By ]
        [ 0;         Vaz - Vcz; Vcy - Vay; 0;       Cz - Az; Ay - Cy ]
    |]
    let b = vector [
        (By * Vbx - Bx * Vby) - (Ay * Vax - Ax * Vay)
        (Cy * Vcx - Cx * Vcy) - (Ay * Vax - Ax * Vay)
        (Bx * Vbz - Bz * Vbx) - (Ax * Vaz - Az * Vax)
        (Cx * Vcz - Cz * Vcx) - (Ax * Vaz - Az * Vax)
        (Bz * Vby - By * Vbz) - (Az * Vay - Ay * Vaz)
        (Cz * Vcy - Cy * Vcz) - (Az * Vay - Ay * Vaz)
    ]

    let OU = M.Solve(b) |> Vector.toArray |> Array.map (round >> int64)

    OU[0] + OU[1] + OU[2]

let (r1, r2) = 
    AOC.Inputs.load "2023" "24" |> Async.RunSynchronously |> parse 
    |> applyBoth (part1 200000000000000.0 400000000000000.0) part2
printfn "Part 1: %A" r1
printfn "Part 2: %A" r2
