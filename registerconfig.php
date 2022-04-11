<?php
/* Database credentials. */
define('DB_SERVER', 'mydb.ics.purdue.edu');
define('DB_USERNAME', 'jone1519');
define('DB_PASSWORD', '');
define('DB_NAME', 'jone1519');
 
/* Attempt to connect to MySQL database */
$link = mysqli_connect(DB_SERVER, DB_USERNAME, DB_PASSWORD, DB_NAME);
 
// Check connection
if($link === false){
    die("ERROR: Could not connect. " . mysqli_connect_error());
}
?>