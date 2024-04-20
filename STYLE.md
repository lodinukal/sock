# Style Notes

- When a structure allocates memory it must make it explicit:
  - where the memory is allocated (pass an allocator)
- A constructor for a structure should be called `create` when it allocates memory, else `init` to allocate from memory or as a POD.
