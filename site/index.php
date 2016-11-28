<?php 
  require_once "func/init.php";
  $title = "Página principal";
  include "template/head.php"; 
  include "template/navbar.php";
?>
    <!-- Main jumbotron for a primary marketing message or call to action -->
    <div class="jumbotron">
      <div class="container">
        <h1>Hello, world!</h1>
        <p>This is our db project! Feel free to do whatever you want</p>
        <p><a class="btn btn-primary btn-lg" href="admin.php" role="button">Go to admin panel &raquo;</a></p>
      </div>
    </div>
<?php include "template/foot.php"; ?>
