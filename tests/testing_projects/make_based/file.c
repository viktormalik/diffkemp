// File for testing of building of snapshots.

int add(int a, int b) {
    return a + b;
}

int mul(int a, int b) {
    int res = 0;
    for (int i = 0; i < b; i++) {
        res = add(res, a);
    }
    return res;
}
