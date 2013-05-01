#!/usr/bin/ruby

# set -v

PATH = "../dist/build/PaperParser/PaperParser"

Dir.chdir "tests"
# ../paper-parser --parse --html http://pubs.acs.org/doi/full/10.1021/ja310432u --out outfile < test_acs.html
# ../paper-parser --formatA --html http://pubs.acs.org/doi/full/10.1021/ja310432u < test_acs.html > outA.html

def with_prefix(prefix,f)
  c = PATH + " --parse --html "+prefix+f+" --out "+f+" < " + f +".html"
  puts c
  system c
end

def with_redirect(prefix,f)
  c = PATH + " --parse --html "+prefix+f+" < " + f +".html > " + f + ".redirect.json"
  puts c
  system c
end

def json_parse(prefix,f)
  c = PATH + " --testjson --html < " + f +".json > " + f + ".parsed.txt"
  puts c
  system c
end

["ja310432u","ja400695h","ja3101215","ja312282g"].each{ |f|
  pre = "http://pubs.acs.org/doi/full/10.1021/"
  with_prefix(pre,f)
  with_redirect(pre,f)
   json_parse(pre,f)
}

["ja309974s-abs"].each{ |f|
  pre = "http://pubs.acs.org/doi/abs/10.1021/"
  with_prefix(pre,f)
  with_redirect(pre,f)
   json_parse(pre,f)
}


Dir.chdir ".."

