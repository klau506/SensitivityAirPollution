from os import walk
path = 'Results/Mort/ZCF_CI/ByRegion/'

filenames = next(walk(path), (None, None, []))[2]  # [] if no file

from PIL import Image

for f in filenames:
    print(f)
    # Fix img size
    img0 = Image.open('extras/blank.png')
    img = img0.resize((2750,4429),Image.ANTIALIAS)

    # Opening the primary image (figure)
    img1_path = path + f
    print(img1_path)
    img1 = Image.open(img1_path)
    img.paste(img1, (0,0))

    # Opening the secondary image (scale)
    img2 = Image.open('extras/legend_bar.png')

    # Pasting img2 image on of img1 
    img.paste(img2, (2225,2849), mask = img2)

    # Displaying the image
    # img1.show()

    # Save the image
    img_name = path + 'withScale/' + f
    img.save(img_name,"PNG")