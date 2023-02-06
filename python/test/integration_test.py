
import pytest
import os

def path_to_bin():
    stream = os.popen('stack exec which haskell-lab3-exe')
    bin_p = stream.read()
    bin_p = bin_p.replace('\n', '')
    return bin_p

def test_1():
    stream = os.popen('cat ./python/test/data/points.txt |'+ path_to_bin() +' --left 1 --right 10 --lm')
    res = stream.read()
    file = open("./python/test/data/expected_result1.txt", "r")
    expected = file.read()
    assert(res == expected)

def test_2():
    stream = os.popen('cat ./python/test/data/points.txt | '+ path_to_bin() +' --left 1 --right 10 --sm')
    res = stream.read()
    file = open("./python/test/data/expected_result2.txt", "r")
    expected = file.read()
    assert(res == expected)

def test_3():
    stream = os.popen('cat ./python/test/data/points.txt | '+ path_to_bin() +' --left 1 --right 10 --lm --sm')
    res = stream.read()
    file = open("./python/test/data/expected_result3.txt", "r")
    expected = file.read()
    assert(res == expected)

def test_4():
    stream = os.popen( path_to_bin() + ' --left 1 --right 10 --sm --file \"./python/test/data/points.txt\"')
    res = stream.read()
    file = open("./python/test/data/expected_result4.txt", "r")
    expected = file.read()
    assert(res == expected)
