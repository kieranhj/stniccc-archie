#!/usr/bin/python
import png,argparse,sys,math,arc

##########################################################################
##########################################################################

def save_file(data,path):
    if path is not None:
        with open(path,'wb') as f:
            f.write(''.join([chr(x) for x in data]))

##########################################################################
##########################################################################

def get_palette(boxed_row_flat_pixel):
    palette = []
    for row in boxed_row_flat_pixel:
        for i in range(0,len(row),4):
            rgb = [row[i+0],row[i+1],row[i+2]]
            if rgb not in palette:
                palette.append(rgb)
            
    return palette

##########################################################################
##########################################################################

def to_box_row_palette_indices(boxed_row_flat_pixel, palette):
    pidxs = []
    for row in boxed_row_flat_pixel:
        pidxs.append([])
        for i in range(0,len(row),4):
            rgb = [row[i+0],row[i+1],row[i+2]]
            idx = palette.index(rgb)
            pidxs[-1].append(idx)

    return pidxs

##########################################################################
##########################################################################

def main(options):
    # Only support MODE 9 for now. MODE 13 coming later.
    if options.mode != 9:
        print>>sys.stderr,'FATAL: invalid mode: %d'%options.mode
        sys.exit(1)

    pixels_per_byte=2
    pack=arc.pack_4bpp

    png_result=png.Reader(filename=options.input_path).asRGBA8()

    width=png_result[0]
    height=png_result[1]
    print 'Image width: {0} height: {1}'.format(width,height)

    palette = get_palette(png_result[2])
    print 'Found {0} palette entries.'.format(len(palette))
    
    if len(palette) > 16:
        print>>sys.stderr,'FATAL: too many colours: %d'%len(palette)
        sys.exit(1)

    # Sort palette by intensity.
    palette.sort(key=lambda e: e[0]*e[0]+e[1]*e[1]+e[2]*e[2])

    if len(palette) < 16:
        # Prefer entry 0 to be black, if not already.
        if palette[0] != [0, 0, 0]:
            palette.insert(0, [0, 0, 0])

        # Pad end of palette with white:
        while len(palette) < 16:
            palette.append([255, 255, 255])

    # Reading the file again seems wrong?
    png_result=png.Reader(filename=options.input_path).asRGBA8()
    pixels = to_box_row_palette_indices(png_result[2], palette)

    pixel_data=[]
    assert(len(pixels)==height)
    for row in pixels:
        assert(len(row)==width)
        for x in range(0,width,pixels_per_byte):
            xs=row[x+0:x+pixels_per_byte]
            assert len(xs)==pixels_per_byte
            pixel_data.append(pack(xs))

    assert(len(pixel_data)==width*height/pixels_per_byte)
    save_file(pixel_data,options.output_path)
    print 'Wrote {0} bytes Arc data.'.format(len(pixel_data))

    if options.palette_path is not None:
        pal_data=[]
        for p in palette:
            warned=False
            for i in range(0,3):
                if (p[i] & 0x0f) != 0 and not warned:
                    print 'Warning: lost precision for colour',p
                    warned=True
                pal_data.append(p[i] & 0xf0)
            pal_data.append(0)
        assert(len(pal_data)==4*len(palette))
        save_file(pal_data,options.palette_path)
        print 'Wrote {0} bytes palette data.'.format(len(pal_data))


##########################################################################
##########################################################################

if __name__=='__main__':
    parser=argparse.ArgumentParser()

    parser.add_argument('-o',dest='output_path',metavar='FILE',help='output ARC data to %(metavar)s')
    parser.add_argument('-p',dest='palette_path',metavar='FILE',help='output palette data to %(metavar)s')
    parser.add_argument('--pad',action='store_true',help='pad palette to 16 entries if fewer')
    parser.add_argument('input_path',metavar='FILE',help='load PNG data from %(metavar)s')
    parser.add_argument('mode',type=int,help='screen mode')
    main(parser.parse_args())
