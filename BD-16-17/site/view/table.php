<?php
function draw_table($table, $title, $classmaker, $heads, $cbck=null, $cbck_args=null, $cbck_const_args=null) {
  echo <<<'ZZZ'
  <div class="row">
    <div class="col-md-12">
      <div class="panel panel-primary">
	<div class="panel-heading">
	  <h3 class="panel-title">
ZZZ
.$title
. <<<'ZZZ'
</h3>
	</div>
ZZZ
. <<<'ZZZ'
        <div class="table-responsive">
	<table class="table table-hover" id="dev-table">
	  <thead>
	    <tr>
              <th>
ZZZ
. implode("</th> <th>", $heads). <<<'ZZZ'
              </th>
	    </tr>
	  </thead>
	  <tbody>
ZZZ;

foreach ($table as $row) {
  echo '
	    <tr ';
  if (!$classmaker===null) {
    call_user_func($classmaker, $row);
  }
  echo'>';
  for ($j=0; $j<count($heads); $j+=1) {
    $params = array();
    $w = 0;
    if ($cbck_args != null && $cbck_args[$j] != null) {
      for ($k=0; $k<count($cbck_args[$j]); $k++) {
        if ($cbck_args[$j][$k]===null) {
          $array_push($params, "null");
        } else if ($cbck_args[$j][$k]===false) {
          if ($cbck_const_args != null && $cbck_const_args[$j] != null &&
              $w < count($cbck_const_args[$j])) {
            $params += $array_push($params, $cbck_const_args[$j][$w]);
          } else {
            $params += $array_push($params, "");
          }
        } else {
          array_push($params, $row[$cbck_args[$j][$k]]);  
        }
      }
    }
    if ($cbck_const_args != null && $cbck_const_args[$j] != null) {
      for (; $w<count($cbck_const_args[$j]); $w++) {
        $params += $array_push($params, $cbck_const_args[$j][$w]);
      }
    }
    if ($cbck==null || ($cbck[$j] == null)) {
      echo "<td>".implode("", $params)."</td>";
    } else {
      echo call_user_func_array($cbck[$j], $params);
    }
  }
  echo '
	    </tr>
';
}
echo '
	  </tbody>
	</table>
      </div>
      </div>
    </div>
  </div>
';
}
?>