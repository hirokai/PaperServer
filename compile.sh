# Compile less
lessc -x ./static/css/welcome.less > ./static/css/welcome.css
lessc -x ./static/css/welcome_mobile.less > ./static/css/welcome_mobile.css
lessc -x ./static/css/duplicated_user.less > ./static/css/duplicated_user.css
lessc -x ./static/css/paperlist.less > ./static/css/paperlist.css
lessc -x ./static/css/format_a.less > ./static/css/format_a.css
lessc -x ./static/css/format_b.less > ./static/css/format_b.css
lessc -x ./static/css/format_a_mobile.less > ./static/css/format_a_mobile.css
lessc -x ./static/css/infoModal.less > ./static/css/infoModal.css
lessc -x ./static/css/paperlist_mobile.less > ./static/css/paperlist_mobile.css

# Compile TypeScript
cd ./static/js
tsc paperlist.ts
cd ../..
