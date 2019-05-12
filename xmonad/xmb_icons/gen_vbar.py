width = 8
height = 24

def split(c):
    return ((c >> 16) & 0xFF, (c >> 8) & 0xff, c & 0xff)

def mix_chan(a, b, f):
    return int(b * f + a * (1.0 - f))

def mix(a, b, f):
    print(f)
    (ra, ga, ba) = split(a)
    (rb, gb, bb) = split(b)
    r = mix_chan(ra, rb, f)
    g = mix_chan(ga, gb, f)
    b = mix_chan(ba, bb, f)
    assert 0 <= r and r <= 0xff
    assert 0 <= g and g <= 0xff
    assert 0 <= b and b <= 0xff

    print(r, g, b)

    return (r << 16) | (g << 8) | b

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

    stops = [
        0xb8b8b8, # base04
        0xdc9656, # base09
        0xab4642, # base08
    ]

    name = 'vbar_%d' % i
    if i <= 4:
        ni = i / 4.0
        color = mix(stops[0], stops[1], ni)
    else:
        ni = (i - 5) / 3.0
        color = mix(stops[1], stops[2], ni)

    print('{:x}'.format(color))

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
