#!/usr/bin/env perl

@lines = `perldoc -u -fatan2`;
foreach (@lines){
    s/\w<([^>]+)>/\U$1/g;
    print;
}
