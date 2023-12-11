import React, { useEffect, useState } from 'react';

function InstagramFeed() {


  useEffect(() => {

    fetch(`https://api.allorigins.win/get?url=${encodeURIComponent('https://www.instagram.com/graphql/query/?query_id=17888483320059182&variables=%7B%22id%22:%227762689736%22,%22first%22:20,%22after%22:null%7D')}`)
      .then(response => {
        if (response.ok) return response.json()
        throw new Error('Network response was not ok.')
      }).then(data => {
        console.log(data.contents)

        // var jsonData = JSON.parse(data.contents);
        // console.log(jsonData)

        //  var items = jsonData.data.user.edge_owner_to_timeline_media.edges;
        //   $.each(items, function(n, item) {

        //           var data_li = "<li><a target='_blank' href='https://www.instagram.com/p/"+item.node.shortcode+"'><img src='" + item.node.thumbnail_src + "'/></a></li>";
        //           $("ul.instagram").append(data_li);

        //   });


        //   foreach ($data->user->edge_owner_to_timeline_media->edges as $item) {

        //     $content_id = $item->node->id; 
        //     $date_posted = $item-node->taken_at_timestamp;
        //     $comments = $item->node->edge_media_to_comment->count;
        //     $likes = $item->node->edge_liked_by->count;
        //     $image = $item->node->display_url;
        //     $content = $item->node->edge_media_to_caption->edges[0]->node->text;

        // }




      });

  }, []);




  return (
    <div className="instapost px-4 md:px-14 py-6 md:py-12">


      <ul className="instagram">
      </ul>



    </div>
  );
}

export default InstagramFeed;
