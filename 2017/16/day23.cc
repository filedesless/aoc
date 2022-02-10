#include <iostream>
#include <vector>
#include <list>

// find the non-primes between b and c
/* to compile
 * mkdir build && cd build
 * cmake ..
 * make
 * ./day23
 */

// old and inefficient method
bool is_prime(int n) {
	for (int d = 2; d != n; d++)
		for (int e = 2; e != n; e++)
			if ( d * e == n )
				return false;
	return true;
}

// faster method by caching content
// and only checking division by primes
bool faster_prime(int n, std::list<int> & memo) {
	if (n <= 2) return true;
	for (auto it = memo.begin(); it != memo.end(); ++it)
		if (n % *it == 0) return false;
	memo.push_back(n);
	return true;
}

int main() {
	int b = 108100;
	int c = 125100;
	int h = 0;
	std::list<int> primes {2};
	std::vector<bool> cache;

	for (int i = 0; i <= c; i++) {
		cache.push_back(faster_prime(i, primes));
		//std::cout << i << " : " << cache.at(i) << std::endl;
	}

	for (int i = 0; i <= 1000; i++) {
		int val = b + 17 * i;
		if ( !cache[val] ) h++;
		//std::cout << "h is " << h << " for " << val << std::endl;
	}

	std::cout << "P2: " << h << std::endl;
	return 0;
}
