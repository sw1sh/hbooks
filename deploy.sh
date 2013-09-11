git checkout -b deploy
cabal clean && cabal configure -fproduction && cabal build
git add -f dist/build/hbooks/hbooks
mkdir libs
cp /usr/local/lib/libMagickCore-6.Q16.so.1 ./libs
cp /usr/local/lib/libMagickWand-6.Q16.so.1 ./libs
git add libs
git commit -m "binary"
git push -f heroku deploy:master
git checkout master
git branch -D deploy
