SNAFU = {'1': 1, '2': 2, '0': 0, '-': -1, '=': -2}
SNAFU_INV = {0: ('0', 0), 1: ('1', 0), 2: ('2', 0), 3: ('=', 1), 4: ('-', 1), 5: ('0', 1)}

def to_dec(s):
    return sum([SNAFU[c] * (5**i) for i, c in enumerate(s[::-1])])

def to_snafu(n):
    snafu = ''
    extra = 0
    while n > 0:
        symbol, extra = SNAFU_INV[n % 5 + extra]
        snafu = symbol + snafu
        n //= 5
    if extra:
        snafu = SNAFU_INV[extra][0] + snafu
    return snafu

def main():
    with open('input.txt') as f:
        data = f.readlines()

    lines = [line.strip() for line in data]
    print(to_snafu(sum([to_dec(line) for line in lines])))

if __name__ == '__main__':
    main()
