<?php

if ($_SERVER['REQUEST_METHOD'] == 'GET' || $_SERVER['REQUEST_METHOD'] == 'POST' ) {
?>
<html>
<head><title>Evidence Accumulation Model Builder</title></head>
<body>

    <p> This is an interface to the Eevidence Accumulation process
    of the Proc4 system.  For more information about Proc4, go to
        <a href="https://pluto.coe.fsu.edu/Proc4/">Proc 4 home
            page.</a></p>

    <p>This script rebuilds the Bayesian network models, rebuilding
        them from a collection of spreadsheets.  This works according
        to the  following steps:</p>
    <ol>
        <li>The latest configuration information and rules are pulled down from
            <a href="https://github.com/ralmond/PP-EA">github</a>.
        The branch listed in the field below is the one which will be
        checked out. </li> 

        <li>The <code>config.json</code> file is read to pick up the details.</li>
        <li>The spreadsheets describing the model are pulled down from
            the net.</li>

        <li>The Bayes nets are rebuilt from the descriptions in the
            spreadsheets. </li>
    </ol>


    <form action="<?php echo $_SERVER['PHP_SELF'] ?>" method="POST">
        App: <input type="url" id="app" name="app" pattern="ecd://.*" required/><br/>
        Branch: <input id="branch" name="Version branch" value="PP-main" required/>
        <br/>
        Administrator: <input type="text" id="aid" name="aid"/ required><br/>
        Password: <input type="password" id="pwd" name="pwd"/ required><br/>
        <input type="submit" name="Do it!"/>
    </form>
    <?php
    if ($_SERVER['REQUEST_METHOD'] == 'POST')
    {
        include 'config.php';
        include 'checkPwd.php';

        $app = $_POST['app'];
        if (strpos($app,'ecd://epls.coe.fsu.edu/') != 0) {
            die("That application is not supported on this server.");
        }

        if (!in_array($app,$INI['apps'])) {
            die("App '$app' not registered.");
        }
        $aid = $_POST['aid'];
        $pwd = $_POST['pwd'];
        $filepwd = get_htpasswd('/usr/local/share/Proc4/p4pwds',$aid);
        if (!matches($pwd,$filepwd)) {
            die("Username or password not matched.");
        }
        $branch = $_POST['branch'];
        $EAhome = $INI['config']['EAHome'];
        $sapp = basename($app);
    ?>
    <h2>Updating configuration <?php echo $branch ?> from github</h2>
    <pre><?php `/usr/local/share/bin/gitconfig $EAhome $branch` ?>
    </pre><br/>
    <?php
    $configfile = file_get_contents($EAhome."/config.json");
    $EAconfig = json_decode($configfile);
    /* Check for Net Directory */
    $netdir = $EAconfig['Tables']['netdir']
    if (is_null($netdir)) {
        $netdir = "nets";
    }
    $netdir = $EAhome."/".$netdir;
    if (!file_exists($netdir)) {
        die("Can't find net directory.")
    }
    $locks = glob($netdir."/*.lock");
    if (count($locks)>0) {
        die("Scoring or rebuilding in process; ".$locks[0]);
    }


    $logdir = $INI['config']['logdir'];
    $logfile = $logdir."/".str_replace("<app>",$sapp,$EAconfig['logname']);
    $lockfile = $netdir."/"."netbuilder.lock";

    if (file_exists($logfile)) {
        unlink($logfile)
    }
    
    exec("/usr/local/share/Proc4/bin/EABuild $sapp $logdir/EAB_$sapp",
         $message, $status);

    if (!$status) {
        header("Refresh: 30; URL=\"EABuilder.php?lockfile=$lockfile&logfile=$logfile\"");
    }
    
    ?>

    <pre><?php echo  $message?></pre>
<?php
} else if ($_SERVER['REQUEST_METHOD'] == 'GET' &&
           !is.null($_GET['lockfile'])) {
    $lockfile = $_GET['lockfile'];
    $logfile = $_GET['logfile'];
    if (file_exists($lockfile)) {
        header("Refresh: 15;
URL=\"EABuilder.php?lockfile=$lockfile&logfile=$logfile\"");
        echo "<p>Builder is still running.</p>";
    } else {
        echo "<p>Builder has finished</p>";
    }
?>
<pre><?php `cat $logfile` ?></pre>
<?php
}
?>
<h2>Links to Other Pages</h2>
<ul>
    <li> <a href=Status.php">status</a> page.</li>
    <li> <a href=Shutdown.php">Shutdown</a> page.</li>
    <li> <a href=EIBuilder.php">Evidence Identification (EI)
        Builder (Loader)</a>.</li>
    <li> <a href=EABuilder.php">Evidence Accumulation (EA)
        Net Builder</a>.</li>
    <li> <a href=EIEvent.php">Evidence Identification (EI)
        Launcher</a>.</li>
    <li> <a href=EABN.php">Evidence Accumulation (EA)
        Launcher</a>.</li>
</ul>
</body>
</html>

<?php
} elseif($_SERVER['REQUEST_METHOD']=="OPTIONS") {
    header('Access-Control-Allow-Credentials: true');
    header('Access-Control-Allow-Headers: Content-Type, Accept, access-control-allow-credentials, access-control-allow-headers, access-control-allow-methods, access-control-allow-origin, access-control-max-age, X-Access-Token, X-Application-Name, X-Request-Time, X-Powered-by');
    header('Access-Control-Allow-Methods: GET, POST, OPTIONS');
    header('Access-Control-Allow-Origin: *');
    header('Access-Control-Max-Age: 1728000');
    header('Content-Length: 0');
    header('Content-Type: text/plain');
} else {
    die("This script only works with GET and POST requests.");
}
?>

