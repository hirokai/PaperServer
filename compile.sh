# Compile less
cd static/css

lessc -x welcome.less > welcome.css.temp
yui-compressor -o welcome.css welcome.css.temp

lessc -x duplicated_user.less > duplicated_user_compiled.css.temp
yui-compressor -o duplicated_user.css duplicated_user_compiled.css.temp

lessc -x paperlist.less > paperlist.css

lessc -x format_a.less > format_a.css

lessc -x format_b.less > format_b.css

lessc -x format_a_mobile.less > format_a_mobile.css

lessc -x infoModal.less > infoModal.css

lessc -x paperlist_mobile.less > paperlist_mobile.css

lessc -x activity.less > activity.css.temp
yui-compressor -o activity.css activity.css.temp

rm *.css.temp

# Compile TypeScript
#cd ../js
#tsc paperlist.ts
#cd ../..
$ yui-compressor -o small_and_compiled_example.css compiled_example.css