import { customTruncateHandler } from './helper';

export const clientEmailLayout = (data: any) => {
  console.log(data, 'inside email');
  const transaction = customTruncateHandler(data?.transaction_id, 15);
  if (data.type === 'newsletter') {
    return `
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
        </head>
        <body>
        <p>Thank you for subscribing to the Xoltan Marketplace!</p>
        </body>    
        `;
  }

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

                    background-color: #353333;
                    border-radius: 2rem;
                    margin: 5rem auto;
                    padding: 1rem;
                }

                .logo-container {

                    background-color: #3f3e3e;
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

                .msg-main {
                    font-size: 1.25rem;
                    margin-top: 3rem;
                    margin-bottom: 3rem;

                }

                .msg-lower-container {
                    margin-bottom: 2rem;
                }

                .verfy-container {
                    text-align: center;
                    color: #fff;
                    margin-bottom: 3rem;
                }

                .verfy-msg {
                    font-weight: 700;
                    font-size: 1.25rem;
                    margin: 2rem 0;
                }

                .verfy-code-container {
                    background-color: #3f3e3e;
                    border-radius: 1rem;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    text-align: center;
                    padding: 0.25rem;   
                    margin: auto 60px;
                }
                .msg-code-container {
                background-color: #3f3e3e;
                border-radius: 1rem;
                color:#fff !important;
                text-align: start;
                /* align-items: center; */
                
                padding: 0.5rem 2rem; 
                font-size: 1.5rem;
                margin: auto 60px;
                }

                .verfy-code {
                    color:#ffffff;
                    font-size: 1.5rem;
                    font-weight: 700;
                }

                .footer-container {
                    background-color: transparent;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    z-index: -2;
                }

                .footer-position{
                    position: relative;
                    margin-top: 3rem;
                    padding:0;
                    margin-bottom: 0;
                    
                }
                
                .footer-logo {
                    position: absolute;
                    bottom: -1rem;
                    right:-0.5rem;
                    z-index: 1;
                }
                
                .footer-msg{
                    color: #FFF;
                    text-align: center;
                    font-size: 0.85rem;
                    line-height: 30px;
                    margin:2rem 0;
                    z-index: 100;
                    position: relative;    
                    
                }

                .customer-support{

                    color:#BF1065;
                    text-decoration: underline;
                }

                .last-msg{
                    position: relative;
                    z-index: 100;
                    bottom:0;
                    text-align: center;
                    color:#857e7e
                }

                .pos{
                    margin: 70px auto;
                }

                .add_gap{
                    margin:60px 0;
                    
                }
                .space{
                    padding:5px 0;
                }
                .nft-img{
                    border-radius:1rem;
                    object-fit:contain;
                }

                .offer_style{
                    font-size:14px;
                }
                .congrats{
                    fonts-size:20px !important;
                    font-weight:bold;
                    text-align:center;
                }
                
            </style>
        </head>

        <body>
            <div class="main-container">
                <!-- mail layout -->
                <div class="logo-container">
                    
                <!-- logo -->
                    <div class="logo">
                        ${
                          data.type == 'project-invitation'
                            ? ` <img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTF8XQKqETBF1ZdlluuFajPNtiL05NucyGnuN-87OmwPYnTyEcRnwRCrb4p9sZakJZkKFA&usqp=CAU" alt="/" />`
                            : data?.offer || data?.buy
                            ? `<img src=${
                                process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL +
                                '/' +
                                data?.nftUpdate?.thumb
                              }  width="300" height="300" class='nft-img' alt='/'/>`
                            : `<img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTF8XQKqETBF1ZdlluuFajPNtiL05NucyGnuN-87OmwPYnTyEcRnwRCrb4p9sZakJZkKFA&usqp=CAU" alt="/" />`
                        }
                    </div>
                </div>

                <!-- msg -->
                <div class="msg-container">
                ${
                  data.otp && data.type == 'otp'
                    ? `<p class="msg-main" >Welcome! Verify your account with our OTP.</p>`
                    : ''
                }
                </div>

                <!-- verification -->
                <div class="verfy-container">
                    ${
                      data.type === 'project-invitation'
                        ? `<p class="verfy-msg" style="text-align:center;">Project Invitation</p>`
                        : ''
                    }
                    
                    <!-- code -->
                    ${
                      data.type === 'project-invitation'
                        ? `<div class="verfy-code-container">${data.raw}
                        </div>`
                        : data?.offer && data.type === 'offer'
                        ? `<div class="msg-code-container add_gap " style='color:#fff !important; '>
                                <p style="color:#fff; font-size:1.5rem; font-weight:700; line-height:30px; text-align:center; padding-bottom:10px;">Congratulations! Your Offer is Accepted</p>
                                <p class='offer_style' style="color:#fff; padding-bottom:10px;">Store Name: ${
                                  data?.store_name
                                }</p>
                                <p class='offer_style' style="color:#fff; padding-bottom:10px;">Accepted by: ${
                                  data?.previous_owner_name
                                }</p>
                                <p class='offer_style' style="color:#fff; line-height:20px; padding-bottom:10px;">Offer Amount: ${(data?.offer_amount).toLocaleString(
                                  'en-US',
                                  {
                                    style: 'currency',
                                    currency: 'USD',
                                    minimumFractionDigits: 2,
                                    maximumFractionDigits: 2,
                                  },
                                )} 
                                <p class='offer_style' style="color:#fff; line-height:20px; padding-bottom:10px;">NFT Name: ${
                                  data?.nftUpdate?.name
                                }</p>
                                <p class='offer_style' style="color:#fff; line-height:20px; padding-bottom:10px;">Token ID: ${
                                  data?.nftUpdate?.token_id
                                }</p>
                                <p class='offer_style' style="color:#fff; line-height:20px; padding-bottom:10px;">View your transaction ID: <a target="_blank" href='${
                                  process.env.NEXT_PUBLIC_POLYGON_TESTNET_URL
                                }/tx/${
                            data?.transaction_id
                          }'>${transaction}</a></p>
                        </div>`
                        : data?.buy && data.type === 'buy'
                        ? `<div class="msg-code-container add_gap " style='color:#fff !important; '>
                        <p style="color:#fff; font-size:1.5rem; font-weight:700; line-height:30px; text-align:center; padding-bottom:10px;">Congratulations! Your Bought This NFT!</p>
                        <p class='offer_style' style="color:#fff; padding-bottom:10px;">Store Name: ${
                          data?.store_name
                        }</p>
                        <p class='offer_style' style="color:#fff; padding-bottom:10px;">Accepted by: ${
                          data?.previous_owner_name
                        }</p>
                        <p class='offer_style' style="color:#fff; line-height:20px; padding-bottom:10px;">Offer Amount: ${(data?.price).toLocaleString(
                          'en-US',
                          {
                            style: 'currency',
                            currency: 'USD',
                            minimumFractionDigits: 2,
                            maximumFractionDigits: 2,
                          },
                        )} 
                        <p class='offer_style' style="color:#fff; line-height:20px; padding-bottom:10px;">NFT Name: ${
                          data?.nft_name
                        }</p>
                    <p class='offer_style' style="color:#fff; line-height:20px; padding-bottom:10px;">Token ID: ${
                      data?.nftUpdate?.token_id
                    }</p>
                        <p class='offer_style' style="color:#fff; line-height:20px; padding-bottom:10px;">View your transaction ID: <a target="_blank" href='${
                          process.env.NEXT_PUBLIC_POLYGON_TESTNET_URL
                        }/tx/${data?.transaction_id}'>${transaction}</a></p>
                </div>`
                        : data.type === 'create'
                        ? `<div class="msg-code-container add_gap " style='color:#fff !important; '>
                                    <p style="color:#fff; font-size:1.25rem; line-height:20px; font-weight:700; text-align:center; padding-bottom:10px;">Congratulations! Your Website has been built.</p>
                                    <p class='offer_style' style="color:#fff; padding-bottom:10px; line-height:30px;">Your Link:${data?.link}</p>
                            </div>`
                        : data.type === 'create-waiting'
                        ? `<div class="msg-code-container add_gap " style='color:#fff !important; '>
                                    <p style="color:#fff; font-size:1.25rem; line-height:20px; font-weight:700; text-align:center; padding-bottom:10px;">Your Website is being built! Please wait for 10 to 15 minutes.</p>
                            </div>`
                        : data?.email && !data?.type
                        ? `<div class="msg-code-container add_gap">
                                        <p class='space'>Email: ${
                                          data.email
                                        }</p>
                                        ${
                                          data.title
                                            ? `<p class='space'>Title: ${data.title}</p>`
                                            : ''
                                        }
                                        ${
                                          data.message
                                            ? `<p class='space'>Message: ${data.message}</p>`
                                            : ''
                                        } 
                                </div>`
                        : ''
                    }
                </div>

                <!-- footer logo -->
                <div class='pos'>
                    <div class="footer-container " >
                        <img src="${
                          process.env.MEDIA_URL
                        }/xoltan-marketplace.png" width='180'  alt="/" class='logo'/>
                    </div>

                    <div class="footer-msg">
                        <p style="text-align:center;">If you need help with anything, kindly contact out </p>
                        <a href="mailto:" class="customer-support">customer support.</a>
                    </div>

                </div>

                <p class="last-msg">This is an auto generate email. Please don't reply to it.</p>
            </div>
        </body>
        </html>
        `;
};
