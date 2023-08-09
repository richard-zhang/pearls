#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstring>
using namespace std;

int N, target;
double dist[20][20], memo[1<< 16];

double matching(int bitmask)
{
    if (memo[bitmask] > - 0.5)
        return memo[bitmask];
    if (bitmask == target)
        return memo[bitmask] = 0;
    
    double ans = 20000000.0;
    int p1, p2;
    for(p1 = 0; p1 < 2 * N; p1++)
    {
        if (!(bitmask & (1 << p1)))
            break;
    }
    for(p2 = p1 + 1; p2 < 2 * N; p2++)
    {
        if (!(bitmask & (1 << p2)))
        {
            ans = min(ans, dist[p1][p2] + matching(bitmask | (1 << p1) | (1 << p2)));
        }
    }
    return memo[bitmask] = ans;
}