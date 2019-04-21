let load_array filename = 

  let img = ImageLib.openfile filename in
  let w = img.Image.width in
  let h = img.Image.height in
  let max_val = img.Image.max_val in

  Array.init h (fun y ->  
    Array.init w (fun x ->
      Image.read_rgb img x y 
        (fun r g b -> 
           r * 256 * 256 * 255 / max_val + 
           g * 256 * 255 / max_val +
           b * 255 / max_val)
    )
  )

