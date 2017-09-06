bnfc flit.bnfc -m && sed -i 's/    -- at end of file.*$/| line t1 == line t0 -> moveAlong st [t0] ts/' LayoutFlit.hs && mv TestFlit.hs{.bak,} && make
