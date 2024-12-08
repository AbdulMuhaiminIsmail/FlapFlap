import os 

def readIMF():
    musicFiles = os.listdir("Assets/Music")
    for musicFile in musicFiles:
        if musicFile.endswith(".imf"):
            with open(f"Assets/Music/{musicFile}", 'rb') as source:
                destMusicFile = musicFile.replace(".imf", "") + ".asm"
                with open(f"Code/MusicData/{destMusicFile}", 'w') as destination:
                    count = 0
                    destination.write("imfData: db ")
                    command = source.read(4)

                    while command:
                        count += 1
                        register = command[0]
                        value = command[1]
                        delayLow = command[2]
                        delayHigh = command[3]
                        
                        # Check if it's the last command to avoid an extra comma
                        if count > 1:
                            destination.write(", ")
                        
                        destination.write(f"0x{register:02X}, 0x{value:02X}, 0x{delayLow:02X}, 0x{delayHigh:02X}")
                        command = source.read(4)

                    # Write imfCount as a decimal value (dw for word)
                    destination.write(f"\n\nimfCount: dw {count}")

# Run the function
readIMF()
