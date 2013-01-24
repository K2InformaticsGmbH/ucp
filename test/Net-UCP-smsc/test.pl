#!/usr/bin/perl

use Net::UCP;

$ucp = Net::UCP->new(FAKE => 1);


my $m = "\2" . "01/00154/O/51/00393201001/10412614190438AB4D/////////////////3//53686F7274204D65737361676520666F72204E454D5558206279204E65743A3A554350\////1////5039/////C7" . "\3";

print "$m\n";

if ($m =~ /^\002(.*)\003$/) {
    my $mess = $1;
    print "$mess\n";

    my $p = $ucp->parse_message($mess) or die "Could not parse the message\n";

    print "$p->{ot}\n";
}

exit 0;
