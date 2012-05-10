#!/usr/bin/perl

use Encode;   #you need it to convert text to GSM 03.38 Default Alphabet. See DATA CODING section
use Net::UCP;
    
my ($acknowledge, $error_number, $error_text);

print "New UCP\n";

$emi = Net::UCP->new(SMSC_HOST   => '127.0.0.1',
		     SMSC_PORT   => '5000',
		     SENDER_TEXT => 'MyApp',
		     # SRC_HOST    => '127.0.0.1',   #optional see below
		     # SRC_PORT    => '5002',        #optional see below
		     WARN        => 1,
		     FAKE        => 0
    ) or die("Failed to create SMSC object");
    
print "open_link\n";

$emi->open_link() or die($!);

print "log in\n";

($acknowledge,$error_number,$error_text) = $emi->login(
    #SMSC_ID    => 'your_account_id',
    #SMSC_PW    => 'your password',
    #SHORT_CODE => 'your Auth Code',

    SMSC_ID    => 14,
    SMSC_PW    => 15,
    SHORT_CODE => 16,

    #OTON       => '2',        #optional
    #ONPI       => '1',        #optional 
    #VERS       => '0100',     #optional
    );
    
print ("Login to SMSC failed. Error nbr: $error_number, Error txt: $error_text\n") unless($acknowledge);
    
print "send sms\n";

($acknowledge,$error_number,$error_text) = $emi->send_sms(
    RECIPIENT      => 033,     #mand.
    MESSAGE_TEXT   => "hello", #opt
    SENDER_TEXT    => 044,     #opt
    );
    
print ("Sending SMS failed. Error nbr: $error_number, Error txt: $error_text\n") unless($acknowledge);

print "close_link\n";
    
$emi->close_link();

exit 0;
