structure CLA = CommandLineArgs

val num_domains = Concurrency.numberOfProcessors

val n = CLA.parseInt "n" 500
val num_bodies = CLA.parseInt "num_bodies" 1024

val pi = 3.141592653589793
val solar_mass = 4.0 * pi * pi
val days_per_year = 365.24

type planet =
  { x: real ref,  y: real ref,  z: real ref
  , vx: real ref, vy: real ref, vz: real ref
  , mass: real
  }

fun aux_1 bodies dt =
  let
  in
    ForkJoin.parfor (num_bodies div num_domains) (0, num_bodies) (fn i =>
      let
        val b = Array.sub (bodies, i)
      in
        Util.for (0, Array.length bodies) (fn j =>
          let
            val b' = Array.sub (bodies, j)
          in
            if i <> j then
              let
                val dx = !(#x b) - !(#x b')
                val dy = !(#y b) - !(#y b')
                val dz = !(#z b) - !(#z b')
                val dist2 = dx * dx + dy * dy + dz * dz
                val mag = dt / (dist2 * Math.sqrt(dist2))
              in
                #vx b := !(#vx b) - dx * #mass b' * mag;
                #vy b := !(#vy b) - dy * #mass b' * mag;
                #vz b := !(#vz b) - dz * #mass b' * mag;

                (* this is racy??? copied exactly from ocaml source... *)
                #vx b' := !(#vx b') + dx * #mass b * mag;
                #vy b' := !(#vy b') + dy * #mass b * mag;
                #vz b' := !(#vz b') + dz * #mass b * mag
              end
            else ()
          end)
      end);

    Util.for (0, num_bodies) (fn i =>
      let
        val b = Array.sub (bodies, i)
      in
        #x b := !(#x b) + dt * !(#vx b);
        #y b := !(#y b) + dt * !(#vy b);
        #z b := !(#z b) + dt * !(#vz b)
      end)

  end

fun energy bodies =
  let
    val e = ref 0.0
  in
    Util.for (0, Array.length bodies) (fn i =>
      let
        val b = Array.sub (bodies, i)
      in
        e := !e + 0.5 * #mass b *
          (!(#vx b) * !(#vx b) + !(#vy b) * !(#vy b) + !(#vz b) * !(#vz b));

        Util.for (i+1, Array.length bodies) (fn j =>
          let
            val b' = Array.sub (bodies, j)
            val dx = !(#x b) - !(#x b')
            val dy = !(#y b) - !(#y b')
            val dz = !(#z b) - !(#z b')
            val distance = Math.sqrt (dx * dx + dy * dy + dz * dz)
          in
            e := !e - (#mass b * #mass b') / distance
          end)
      end);

    !e
  end

fun offset_momentum bodies =
  let
    val px = ref 0.0
    val py = ref 0.0
    val pz = ref 0.0
  in
    Util.for (0, Array.length bodies) (fn i =>
      let
        val b = Array.sub (bodies, i)
      in
        px := !px + !(#vx b) * #mass b;
        py := !py + !(#vy b) * #mass b;
        pz := !pz + !(#vz b) * #mass b
      end);
    #vx (Array.sub (bodies, 0)) := ~ (!px) / solar_mass;
    #vy (Array.sub (bodies, 0)) := ~ (!py) / solar_mass;
    #vz (Array.sub (bodies, 0)) := ~ (!pz) / solar_mass
  end

val seed = Random.rand (42, 15210)
fun randFloat bound =
  bound * (Random.randReal seed)

val bodies =
  Array.tabulate (num_bodies, fn _ =>
    { x = ref (randFloat 10.0)
    , y = ref (randFloat 10.0)
    , z = ref (randFloat 10.0)
    , vx = ref (randFloat 5.0 * days_per_year)
    , vy = ref (randFloat 4.0 * days_per_year)
    , vz = ref (randFloat 5.0 * days_per_year)
    , mass = randFloat 10.0 * solar_mass
    })

val _ = offset_momentum bodies
val _ = print ("initial energy: " ^ Real.toString (energy bodies) ^ "\n")

val _ = Benchmark.run "running simulation" (fn _ =>
  Util.for (0, n) (fn _ => aux_1 bodies 0.01))

val _ = print ("final energy: " ^ Real.toString (energy bodies) ^ "\n")
