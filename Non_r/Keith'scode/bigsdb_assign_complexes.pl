#!/usr/bin/perl
#This script assigns allelic profiles to clonal
#complexes in an MLST database.  It reads in a
#file containing a list of complex central genotypes
#whose order decides the priority of assignment.
#Assignments are built up, such that all profiles
#matching at 6 loci are assigned for all complexes, then
#all matching at 5 etc.  If a profile has already been
#assigned to another complex, it will not be re-
#assigned (to prevent problems with overlapping
#complexes), although all assignments are reset at the
#beginning of the sweep.

use DBI;
use XML::Parser::PerlSAX;
use strict;
use 5.010;
my ( $dbase, $scheme_id, $complex_file, $min_matches, $st_field, $cc_field ) = @ARGV;

$min_matches //= 4;
$st_field //= 'ST';
$cc_field //= 'clonal_complex';

if ( !$complex_file ) {
	print "Usage 'bigsdb_assign_complexes.pl <dbase> <scheme_id> <complex_file>\n";
	exit;
}

my $db = DBI->connect( "DBI:Pg:dbname=$dbase", 'postgres' ) or die "couldn't connect " . DBI->errstr;

#open and read complexes file
my ( @complex_st, @complex_name );
if (!-e $complex_file ){
    die "Complex field $complex_file does not exist.\n";
}

open( my $complex_fh,'<', $complex_file ) or die "Cannot open complex file";
my $i = 0;
while ( my $line = <$complex_fh> ) {
    next if !$line;
    chomp $line;
    ( $complex_st[$i], $complex_name[$i] ) = split /\t/, $line;
    $i++;
}
close $complex_fh;


my $sql = $db->prepare("SELECT locus FROM scheme_members WHERE scheme_id=? ORDER BY field_order");
$sql->execute($scheme_id);
my @loci;
while (my ($locus) = $sql->fetchrow_array){
	push @loci,$locus;
}

$" = ',';
my %cc_by_st;
my $locus_count_sql = $db->prepare('SELECT COUNT(*) FROM scheme_members WHERE scheme_id=?');
$locus_count_sql->execute($scheme_id);
my ($locus_count) = $locus_count_sql->fetchrow_array;

say "Locus count: $locus_count; Minimum matches to central: $min_matches";

for ( my $matches = $locus_count ; $matches >= $min_matches ; $matches-- ) {
	print "\nmatches: $matches\n\n";
	my $qry = "SELECT $st_field,profile FROM mv_scheme_$scheme_id ORDER BY $st_field";
	$sql = $db->prepare($qry) or die "couldn't prepare" . $db->errstr;
	my $qry2 = "SELECT profile FROM mv_scheme_$scheme_id WHERE $st_field=?";
	my $sql2 = $db->prepare($qry2) or die "couldn't prepare" . $db->errstr;
	for ( my $i = 0 ; $i < scalar @complex_st ; $i++ ) {
		$sql2->execute( $complex_st[$i] );
		my ($profile) = $sql2->fetchrow_array();
		say "Complex $st_field:$complex_st[$i]\tProfile:@$profile";
		$sql->execute;
		while (my ($ST,$qry_profile) = $sql->fetchrow_array ) {
			my $qry_match   = 0;
			for ( my $j = 0 ; $j < $locus_count ; $j++ ) {
				if ( $profile->[$j] == $qry_profile->[$j] ) {
					$qry_match++;
				}
			}
			if ( ( $qry_match == $matches ) && !$cc_by_st{$ST}   ) {
				print "ST $ST (@$qry_profile) assigned.\n";
				$cc_by_st{$ST} = $complex_name[$i];
			}
		}
	}
}

#Remove all previous assignments
my $qry = "DELETE FROM profile_fields WHERE scheme_id=? AND scheme_field=?";
$sql = $db->prepare($qry) or die "couldn't prepare" . $db->errstr;
$sql->execute($scheme_id,$cc_field);

#Write assigned complexes to database
print "\nWriting to database.\n";
foreach my $ST ( sort byvalue keys %cc_by_st ) {
	$db->do("INSERT INTO profile_fields (scheme_id,scheme_field,profile_id,value,curator,datestamp) VALUES (?,?,?,?,?,?)",undef,$scheme_id,$cc_field,$ST,$cc_by_st{$ST},0,'now') or die "couldn't update st $ST.\n";
}

print "Completed.\n";
$sql->finish;
$locus_count_sql->finish;
$db->disconnect;

sub byvalue {
	$a - $b;
}
