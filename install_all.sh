
py=`which python`
echo "Using python as $py"

list="bufr xml-marshall sqlalchemy-marshall metadb transform"
for p in $list; do
   echo "======= Now in $p"
   cd $p && rm -r build && python setup.py build && python setup.py install && cd ..
   echo "======= Done with $p"
done
