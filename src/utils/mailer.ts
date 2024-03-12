import { customTruncateHandler } from './helper';
import ticketlogo from 'src/public/assets/ticketlogo.svg';
export const clientEmailLayout = (data: any) => {
  console.log(data, 'inside email');

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
            background-color: #FF3634;
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
		.text-main{
          font-family: Arial, sans-serif;
        	color: #000000; /* Black text color */
            font-weight: 600; /* Semibold font weight */
            font-size: 16px; /* Font size around 16px */
        }
        .logo img {
            display: block;
            margin: 0 auto;
        }

        .verfy-container {
            text-align: center;
            color: #fff;
            margin-bottom: 2rem; /* Reduced margin */
        }

        .verfy-msg {
            font-weight: 700;
            font-size: 1.25rem;
            margin: 2rem 0;
        }

        .verfy-code-container {
            background-color: #FFFFFF;
            border-radius: 1rem;
            display: flex;
            align-items: center;
            justify-content: center;
            text-align: center;
            padding: 0.25rem;   
            margin: auto 60px;
        }

        .footer-container {
            background-color: transparent;
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: -2;
        }

        .footer-msg {
            color: #FFF;
            text-align: center;
            font-size: 0.85rem;
            line-height: 30px;
            margin-top: 2rem; /* Reduced margin */
            margin-bottom: 1rem; /* Reduced margin */
            z-index: 100;
            position: relative;    
        }

        .customer-support {
            color: #FFF;
            text-decoration: underline;
            font-size: 22px;
        }

        .last-msg {
            position: relative;
            z-index: 100;
            bottom: 0;
            text-align: center;
            color: #00000;
            margin-top: 0.1rem; /* Reduced margin */
            color: #000000; /* Black text color */
            font-weight: 600; /* Semibold font weight */
            font-size: 16px;
        }

        .pos {
            margin: 10px auto;
        }

    </style>
</head>

<body>
    <div class="main-container">
        <!-- mail layout -->
        <div class="logo-container">
            <!-- logo -->
            <div class="logo">
                <img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTF8XQKqETBF1ZdlluuFajPNtiL05NucyGnuN-87OmwPYnTyEcRnwRCrb4p9sZakJZkKFA&usqp=CAU" alt="/" />
            </div>
        </div>

        <!-- Project invitation -->
        <h1 style="color: #fff; text-align: center; margin-bottom: 2rem;">
        
        ${data.type == 'project-invitation' ? 'Project Invitation' : ''}
        ${data.type == 'platfrom-invitation' ? 'Project Invitation' : ''}
        </h1>

        <!-- verification container -->
        <div class="verfy-container">
            <div class="verfy-code-container"></div>
               <p class="text-main">
               ${
                 data.type == 'project-invitation'
                   ? `<p> ${data.userData} invited you as client in ${data.validate} project. </p>`
                   : ''
               }
        ${
          data.type == 'platfrom-invitation'
            ? `<p> ${
                data.userData?.first_name ?? 'Seller/Buyer'
              } invited you as client in ticketing platform. </p>`
            : ''
        }
               </p> 
            </div>
        </div>

        <!-- footer logo -->
        <div class='pos'>
    <div class="footer-container">
        <img src="${ticketlogo}" width='20' alt="Ticket Logo" class='logo'/>
    </div>

    <div class="footer-msg">
        <p style="text-align:center;font-size: 16px;">If you need help with anything, kindly contact our </p>
        <a href="mailto:" class="customer-support">customer support.</a>
    </div>
</div>

        <p class="last-msg">This is an auto-generated email. Please don't reply to it.</p>
    </div>
</body>
</html>
        `;
};
