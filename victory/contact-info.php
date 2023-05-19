<?php
$firstname = $_POST['firstname'];
$lastname = $_POST['lastname'];
$email = $_POST['email'];
$address = $_POST['address'];
$city = $_POST['city'];
$state = $_POST['state'];
$zip = $_POST['zip'];
$image = $_POST['image'];

$email_from = 'info@victoryu.com';

$email_subject = 'New Recruit';

$email_body = "Name: $firstname $lastname.\n".
                "Email: $email.\n".
                "Address: $address.\n".
                "City: $city.\n".
                "State: $state.\n".
                "Zip: $zip.\n".
                "Image: $image.\n";

$to = 'landon@daines.net';

$headers = "From: $email_from.\r\n";

$headers .= "Reply-To: $email.\r\n";

mail($to, $email_subject, $email_body, $headers);

header("Location: personal.html");

?>