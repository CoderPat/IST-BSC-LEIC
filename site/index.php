<?php 
  require_once "func/init.php";
  $title = "Página principal";
  $root = "./";
  include $root . "template/head.php"; 
  include $root . "template/navbar.php";
?>
    <!-- Main jumbotron for a primary marketing message or call to action -->
    <div class="jumbotron">
      <div class="container">
        <h1>Hello, world!</h1>
        <p>Este é o nosso projeto de db!</p>
        <p><a class="btn btn-primary btn-lg" href="admin.php" role="button">Ir para painel de administração &raquo;</a></p>
      </div>
    </div>
<?php include $root . "template/foot.php"; ?>
