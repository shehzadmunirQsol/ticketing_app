import React, { useEffect, useState } from 'react';
import InstagramEmbed from 'react-instagram-embed';

function InstagramFeed() {


  return (
    <div className="instapost px-4 md:px-14 py-6 md:py-12">
      <InstagramEmbed
        url='https://www.instagram.com/ahmarsm/'
        clientAccessToken='1292895421408352|IGQWRNNzNfNC01RlVQbDlIc01MOVMwR3ZATVXNGeUh3bVhIYzJPYUg3RkhBNWp0OU5UaUUtRThtblhrWXhEakVVdi1uNjZAvV3ZADNXYyR01RZA0FFeko0OEp5eVJpb19jOEZAfTHJwbmlFS19abDZAfbTJ5YlVYUGhkUDgZD'
        maxWidth={320}
        hideCaption={false}
        containerTagName='div'
        protocol=''
        injectScript
        onLoading={() => { }}
        onSuccess={() => { }}
        onAfterRender={() => { }}
        onFailure={() => { }}
      />
    </div>
  );
}

export default InstagramFeed;
