export const clientEmailLayout = (data: any) => {
  console.log(data, 'inside email');
    console.log(data.type==="create-user");
    console.log("data type",data.type);
    console.log("data check",data.usercontent);

  return `
  <!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
        body {
            width: 100%;
            height: 100%;
            margin: auto;
        }

        .main-container {
            width: 100%;
            height: 100%;
            max-width: 500px;
            z-index: -1;
            background-color: #90DDF0;
            border-radius: 2rem;
            margin: 5rem auto;
            padding: 1rem;
        }

        .logo-container {
            background-color: #FFFFFF;
            border-radius: 2rem;
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 1rem;
        }

        .logo {
            margin-left: auto;
            margin-right: auto;
        }

        .msg-container {
            color: #FFF;
            text-align: center;
        }
        .pos{
            width:100%;
            margin:0 auto;
            position:relative;
        }

        .footer-container {
            width: 100%;
            background-color: transparent;
            margin:0 auto;
            position:relative;
        
            text-align:center;
        }

        .footer-logo {
            margin-top: 10px;
            margin:0 auto;
        }

        .footer-msg {
            color: #FFF;
            text-align: center;
            font-size: 0.85rem;
            line-height: 30px;
            margin-top: 10px;
            margin-bottom: 5px;
        }

        .customer-support {
            color: #BF1065;
            text-decoration: underline;
            margin-bottom: 0;
        } 

        .last-msg {
            position: relative;
            bottom: 0;
            text-align: center;
            color: #857e7e;
            margin-top: 5px;
            margin-bottom: 0;
        }
    </style>
</head>
<body>
    <div class="main-container">
        <!-- mail layout -->
        <div class="logo-container">
            <!-- logo -->
            <div class="logo">
            <img src="https://raw.githubusercontent.com/muhammadwaqarqsol/NextEventApp/main/public/assets/email-removebg-preview.png" alt="Email Invitation Icon" width="120" height="120">
            </div>
        </div>

        <!-- msg -->
        <div class="msg-container">
            <h2 style="font-weight: 600;margin-bottom: 0;">
            ${ [
                'platform-seller',
                'platform-client',
                'platform-trucker',
              ].includes(data.type) ? "Platform Invitation":""}

              ${ [
                'project-seller',
                'project-client',
                'project-trucker',
              ].includes(data.type) ? "Project Invitation":""}

              ${ [
                'project-create',
              ].includes(data.type) ? "Project Creation":""}

              ${ [
                'ticket-create',
                'ticket-update',
                'ticket-close',
              ].includes(data.type) ? "Project Ticket":""}

            </h2>
            ${data.usercontent &&  data.usercontent}
        </div>

        <!-- footer logo -->
        <div class='pos'>
            <div class="footer-container" >
                <img src="https://raw.githubusercontent.com/muhammadwaqarqsol/NextEventApp/main/public/assets/ticketlogo.png" width="55" height="55" alt="Ticket Logo" class="footer-logo">
                <p style="margin-bottom: 0;margin-top: 5px;">Ticketing App</p>
            </div>

            <div class="footer-msg">
                <p style="color:#857e7e;font-weight: 600;">If you need help with anything, kindly contact our <a href="mailto:project.ticketing2024@gmail.com" class="customer-support">customer support.</a></p>
            </div>
        </div>

        <p class="last-msg">This is an auto-generated email. Please don't reply to it.</p>
    </div>
</body>
</html>  
        `;
};
