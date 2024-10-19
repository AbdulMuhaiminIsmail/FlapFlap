from PIL import Image

path = "Assets\chuck.jpg"
dimensions = (25, 25)
filename = "chuck.asm"
name = "chuck"

img = Image.open(path).resize(dimensions).convert("P")

pixels = list(img.getdata())
palette = img.getpalette()

with open(filename, "w") as file:
    file.write(name + "Pixels: db ")
    file.write(", ".join(str(pixel) for pixel in pixels))

    file.write("\n\n")

    file.write(name + "Palette: db ")
    file.write(", ".join(str(hex(clr)) for clr in palette))
    file.write("\n")

print("Pixel data of the image has been written to " + filename)
