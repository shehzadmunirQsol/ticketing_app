import dynamic from 'next/dynamic';
import React, { useEffect } from 'react';
import { prisma } from '~/server/prisma';
import parse from 'html-react-parser';
import AboutCarousel from '~/components/app/about/about_carousel';

export async function getStaticPaths() {
  const response: any = await prisma.cMS.findMany({
    select: {
      id: true,
      slug: true,
    },
  });

  const paths = response?.map((post: any) => ({
    params: { id: post?.slug },
  }));

  return { paths, fallback: false };
}
export async function getStaticProps({ params }: any) {
  const response = await prisma.cMS.findFirst({
    where: {
      slug: params?.id,
    },

    select: {
      id: true,
      slug: true,
      type: true,
      CMSDescription: {
        select: {
          content: true,
        },
      },
    },
  });


  return { props: { storeBlogsData: response } };
}


const CmsFunc = dynamic(() => import('~/components/app/cms/index'), {
  ssr: true,
});

const findElementsWithAttribute = (node: any) => {
  if (node.type === 'tag') {
    const shortcode = node?.attribs;

    if (shortcode?.data === 'main-carousel') {
      const imageCrousel = JSON.parse(node?.children[0]?.data) as [];

      if (imageCrousel.length) {
        return <AboutCarousel imageCrousel={imageCrousel} />;
      }
    } else {
      return node;
    }
  }
};

export default function CmsPage({ storeBlogsData }: any) {
  const reactElements = parse(storeBlogsData?.CMSDescription[0]?.content || '', {
    replace: findElementsWithAttribute,
  });

  return (
    <div className=" w-full bg-bg-1 py-2">
      <div className="mx-auto max-w-[1600px]">{reactElements}</div>
    </div>
  );
}
