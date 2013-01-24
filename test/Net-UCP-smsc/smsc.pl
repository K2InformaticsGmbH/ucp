#!/usr/bin/perl

use Net::UCP;

$ucp = Net::UCP->new(FAKE => 1);

sub parse_message {
    my $m = shift;

    print("m = $m\n");

    if ($m) {
	if ($m =~ /^\002(.*)\003$/) {
	    my $mess = $1;
	    my $res = $ucp->parse_message($mess);

	    if ($res) {
		return $res;
	    } else {
		print "Failed to parse message\n";
		return 0;
	    }
	} else {
	    print "Not an UCP string\n";
	    return 0;
	}
    } else {
	print "Empty string\n";
	return 0;
    }
}

sub build_response {
    my $ucp = shift;
    my $mess = shift;

    if ($mess) {
        my $ot = $mess->{ot};
	my $trn = $mess->{trn};
	my $resp;
        if ($ot eq 31 or $ot eq 51 or $ot eq 60) {
	    $resp = $ucp->make_message(op => $ot,
				       trn => $trn,
				       result => '1',
				       ack => 'A',
				       sm => '1:2');
        } else {
	    $resp = $ucp->make_message(op => $ot,
				       trn => $trn,
				       result => '1',
				       nack => 'N',
				       ec => '64',
				       sm => 'Not supported');
        }

	$resp = "\002" . $resp . "\003";

	print "r = $resp\n";

	$resp;
    } else {
        return "";
    }
}

sub ucp_action_callback($) {
    my $ucp_string = shift;	

    print "Got an UCP action callback\n";
    
    my $mess = parse_message($ucp_string);

    return build_response($ucp, $mess);
}

sub ucp_sending_callback($) {
    my $ucp_string = shift;
    
    print "Got an UCP sending callback\n";
    
    my $mess = parse_message($ucp_string);

    return build_response($ucp, $mess);
}

print "Starting SMSC\n";

$ucp->create_fake_smsc(
    host         => "127.0.0.1",
    port         => 5000,
    listen       => 10,
    output       => 1,
    reading_mode => 1,
    max_len      => 2048,
    action       => \&ucp_action_callback,
    sending      => \&ucp_sending_callback
    );

print "SMSC Stopped\n";

exit 0;
