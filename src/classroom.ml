module Unix = struct
  include Unix

  let system _ = failwith "Don't use Unix.system"
end
