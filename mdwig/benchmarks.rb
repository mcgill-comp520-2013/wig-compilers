#! /usr/bin/env ruby

# Forward its arguments to mdwig, so for more informations
# see ./mdwig --help

$failed=""
$failedcount=0
$okcount=0

$arg = ""
ARGV.each { |a| $arg << a << " "}

`find examples -type f -name '*.wig'`.split("\n").each do |wig|
filename = wig.sub( /.*\/(.*)\.wig$/i , '\1')
	msg=`./mdwig.native -o output/#{filename}.rb #{$arg} #{wig} 2>&1`
	if $? == 0 
	then
    puts "."
		$okcount=$okcount + 1
	else
		puts '#'
		$failedcount = $failedcount + 1
		$failed << wig << "\n" << msg
    end
  end


if $failed
then
	puts $failed
	puts $okcount.to_s << " tests succeeded"
	puts $failedcount.to_s <<  " tests failed"
end
