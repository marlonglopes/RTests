#!/usr/bin/env ruby

require 'rubygems'
require 'aws-sdk'

bucket_name = '*** Provide bucket name ***'
file_name = '*** Provide file name ****'

# Get an instance of the S3 interface.
s3 = AWS::S3.new

# Upload a file.
key = File.basename(file_name)
s3.buckets[bucket_name].objects[key].write(:file => file_name)
puts "Uploading file #{file_name} to bucket #{bucket_name}."