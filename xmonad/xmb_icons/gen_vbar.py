width = 8
height = 12

def gen_icon(i):
    # /* XPM */
    # static char *vbar_0[] = {
    # /* columns rows colors chars-per-pixel */
    # "21 24 2 1 ",
    # "  c white",
    # ". c black",
    # /* pixels */
    # "pixel data",
    # "separated by rows"
    # };

    name = 'vbar_%d' % i
    if i < 2:
        color = 0x000000
    elif i < 4:
        color = 0x85990
    else:
        color = 0xdc322f
    with open(name + '.xpm', 'w') as outf:
        outf.write('/* XPM */\n')
        outf.write('static char *' + name + '[] = {\n')
        outf.write('/* columns rows colors chars-per-pixel */\n')
        outf.write('"%d %d 2 1",\n' % (width, height))
        outf.write('"  c none",\n')
        outf.write('". c #%06x",\n' % color)
        outf.write('/* pixels */\n')

        for y in range(height):
            if y != 0:
                outf.write('",\n')
            outf.write('"')
            for x in range(width):
                if (height - y - 1) * 8 <= i * height:
                    outf.write('.')
                else:
                    outf.write(' ')
        outf.write('"\n');
        outf.write('}')

for i in range(0, 9):
    gen_icon(i)
