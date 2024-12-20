#!/usr/bin/env ruby

require 'json'

if ARGV.length != 2
  puts "Usage: #{$0} <first_results.json> <second_results.json>"
  exit 1
end

first_file, second_file = ARGV

begin
  first_results = JSON.parse(File.read(first_file))
  second_results = JSON.parse(File.read(second_file))
rescue JSON::ParserError => e
  puts "Error parsing JSON files: #{e.message}"
  exit 1
rescue Errno::ENOENT => e
  puts "Error reading files: #{e.message}"
  exit 1
end

count = first_results.inject(0) { |sum, r| sum + (r['result']['pass'] ? 1 : 0) }
puts "First: #{count}"
count = second_results.inject(0) { |sum, r| sum + (r['result']['pass'] ? 1 : 0) }
puts "Second: #{count}"

# Create lookup hash for second results
second_lookup = second_results.each_with_object({}) do |result, hash|
  hash[result['file']] = result['result']['pass']
end

# Find tests that passed in first but failed in second
regressions = first_results.select do |result|
  result['result']['pass'] && second_lookup.key?(result['file']) && !second_lookup[result['file']]
end

if regressions.empty?
  puts "No regressions found!"
else
  puts "Tests that passed in #{first_file} but failed in #{second_file}:"
  regressions.each do |result|
    puts result['file']
  end
  puts "\nTotal regressions: #{regressions.length}"
end
