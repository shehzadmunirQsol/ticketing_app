import React from 'react';
import Frame11 from '~/public/assets/icons/Frame11.svg';
import Frame12 from '~/public/assets/icons/Frame12.svg';
import Frame13 from '~/public/assets/icons/Frame13.svg';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import NextImage from '~/components/ui/img';
import { useState } from 'react';
import { trpc } from '~/utils/trpc';

interface productInterface {
    item?: any;
    lang: number;
}
function EventDesc(props: productInterface) {

    const [eventDataWithoutLang, setEventDataWithoutLang] = useState<any>({});

    // GET EVENT ARABIC / ENGLISH DATA ---  
    trpc.cart.getEventDataByCartID.useQuery(
        { event_id: props?.item?.event_id },
        {
            onSuccess(data: any) {
                if (data?.data) {
                    console.log(data.data, "data.data ")
                    setEventDataWithoutLang(data.data ?? [])
                }
            },
            onError(error: any) {
                console.log({ error });
            },
        },
    );
    // GET EVENT ARABIC / ENGLISH DATA ---


    var langName =  eventDataWithoutLang ? eventDataWithoutLang?.EventDescription?.find((description: { lang_id: number }) => description.lang_id === props.lang)?.name : "";

    return (
        <div
            className="flex flex-row justify-between "
            key={props?.item.id}
        >
            <p className="lg:text-2xl md:lg:text-xl   w-[60%]">
                {/* {props?.item?.Event?.EventDescription[0]?.name} */}
                {langName}
            </p>
            <p className="font-black text-lg lg:text-xl ">
                AED{' '}
                {(props?.item?.Event?.price * props?.item?.quantity)?.toFixed(
                    2,
                )}
            </p>
        </div>
    );
}

export default EventDesc;

