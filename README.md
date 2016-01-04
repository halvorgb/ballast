# ballast
Setup
```
git clone git@github.com:sbidin/sdl2-image.git libs/sdl2-image
git clone git@github.com:sbidin/sdl2-ttf.git libs/sdl2-ttf
git clone git@github.com:sbidin/sdl2-mixer.git libs/sdl2-mixer
cabal sandbox init
cabal sandbox add-source libs/sdl2-image
cabal sandbox add-source libs/sdl2-ttf
cabal sandbox add-source libs/sdl2-mixer
cabal install --only-dependencies
```

Run example: 
```
cabal run
```
