function Image (img)
  img.caption[1] = pandoc.Strong(img.caption[1])
  img.caption[3] = pandoc.Strong(img.caption[3])
  img.caption[4] = pandoc.Strong(".-- ")
  return img
end