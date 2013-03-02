#include <iostream>

extern "C"
{
    void testFFI_c(int a)
    {
        std::cout << a << std::endl;
    }
}