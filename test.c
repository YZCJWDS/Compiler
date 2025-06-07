int set(int a[], int pos, int d)
{
    int x[31];

    int k = 0;
    while (k < 31) {
        x[k] = 0;
        k = k + 1;
    }

    x[0] = 1;
    x[1] = x[0] * 2;
    x[2] = x[1] * 2;
    x[3] = x[2] * 2;
    x[4] = x[3] * 2;
    x[5] = x[4] * 2;
    x[6] = x[5] * 2;
    x[7] = x[6] * 2;
    x[8] = x[7] * 2;
    x[9] = x[8] * 2;
    x[10] = x[9] * 2;

    int i = 10;
    while (i < 30) {
        i = i + 1;
        x[i] = x[i - 1] * 2;
    }

    int v = 0;

    if (pos / 30 >= 10000)
        return 0;

    if (a[pos / 30] / (x[pos % 30]) % 2 != d) {
        if (a[pos / 30] / (x[pos % 30]) % 2 == 0)
            if (d == 1)
                v = x[pos % 30];

        if (a[pos / 30] / x[pos % 30] % 2 == 1)
            if (d == 0)
                v = v - x[pos % 30];
    }

    a[pos / 30] = a[pos / 30] + v;
    return 0;
}

int seed[3];
int staticvalue;

int rand()
{
    staticvalue = staticvalue * seed[0] + seed[1];
    staticvalue = staticvalue % seed[2];
    if (staticvalue < 0)
        staticvalue = seed[2] + staticvalue;
    return staticvalue;
}

int a[10000];
int main()
{
    seed[0] = 19971231;
    seed[1] = 19981013;
    seed[2] = 1000000000 + 7;
    int n = getint();
    staticvalue = getint();
    int x, y;
    while (n > 0) {
        n = n - 1;
        x = rand() % 300000;
        y = rand() % 2;
        set(a, x, y);
    }
    putarray(10000, a);
    return 0;
}
