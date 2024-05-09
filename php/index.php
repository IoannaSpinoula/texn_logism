<?php include_once 'header.php' ?>
<!DOCTYPE html>
<html lang="en">

<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>

<body class="index" >
    <section class="names">
        <h2>Upload your data</h2>

        <form action="/upload" method="post" enctype="multipart/form-data">
            <input type="file" name="fileToUpload" id="fileToUpload">
            <br>
            <input type="submit" value="Upload File">
        </form>
    </section>

</body>
</html>
