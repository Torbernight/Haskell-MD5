# MD5 hashing algorithm implemented purely in Haskell

Small project I made for fun to wrap up what I've been learning from the Haskell programming language.

No AI has been used in the programming or the redacting of comments, all was made by me.

## Build
    ghc MD5.hs -o MD5

## Usage:
    ./MD5 {input file} # to compute the digest of a file's content    
    ./MD5              # to compute the digest of STDIN input

## examples:
    $ ./MD5 /dev/null
    d41d8cd98f00b204e9800998ecf8427e 

    $ echo "The quick brown fox jumps over the lazy dog" | ./MD5 
    9e107d9d372bb6826bd81d3542a419d6
