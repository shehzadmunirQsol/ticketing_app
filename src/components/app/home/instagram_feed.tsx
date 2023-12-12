import React, { useEffect, useState } from 'react';
import { ElfsightWidget } from 'react-elfsight-widget';

function InstagramFeed() {
  const [instaPost, setInstaPost] = useState([]);

  return (
    <div className="instapost px-4 md:px-14 py-6 md:py-12">

    {/* <script src="https://static.elfsight.com/platform/platform.js" data-use-service-core defer></script>
    <div className="elfsight-app-bd69b607-c562-42c6-9b4a-8ce9b3b410b5" data-elfsight-app-lazy></div> */}


    <ElfsightWidget widgetId="bd69b607-c562-42c6-9b4a-8ce9b3b410b5" />
     

      {/* <ul className="instagram">
        {
          instaPost ?
            instaPost.map((insta) => {
              return (
                <li>
                  <a target='_blank' href={`https://www.instagram.com/p/${insta.node.shortcode}`}>
                    <img src={insta.node.display_url} />
                  </a>
                </li>
              );
            })
            :
            null
        }
      </ul> */}




    </div>
  );
}

export default InstagramFeed;
