#ifndef HASH_H
#define HASH_H

#include <string>

namespace artic {

/// FNV: Initialization value.
inline uint32_t hash_init() { return 0x811C9DC5; }

/// FNV: Hashes one byte.
inline uint32_t hash_combine(uint32_t h, uint8_t d) {
    return (h * 16777619) ^ d;
}

/// FNV: Hashes one word.
inline uint32_t hash_combine(uint32_t h, uint16_t d) {
    h = hash_combine(h, uint8_t(d));
    h = hash_combine(h, uint8_t(d >> 8));
    return h;
}

/// FNV: Hashes one double word.
inline uint32_t hash_combine(uint32_t h, uint32_t d) {
    h = hash_combine(h, uint16_t(d));
    h = hash_combine(h, uint16_t(d >> 16));
    return h;
}

/// FNV: Hashes one double word.
inline uint32_t hash_combine(uint32_t h, uint64_t d) {
    h = hash_combine(h, uint32_t(d));
    h = hash_combine(h, uint32_t(d >> 32));
    return h;
}

/// FNV: Hashes a string.
inline uint32_t hash_combine(uint32_t h, const std::string& str) {
    for (auto& c : str) h = hash_combine(h, uint8_t(c));
    return h;
}

/// FNV: Hashes several objects.
template <typename T, typename... Args>
inline uint32_t hash_combine(uint32_t h, T t, Args... args) {
    return hash_combine(hash_combine(h, t), args...);
}

} // namespace artic

#endif // HASH_H
