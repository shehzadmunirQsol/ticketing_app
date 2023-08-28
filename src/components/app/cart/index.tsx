import Image from 'next/image';
import CarImage from '~/public/assets/Ford-Mustang-PNG-Pic.png';
import BottleImage from '~/public/assets/bottle.png';
import { Button } from '~/components/ui/button';
import { Switch } from '~/components/ui/switch';
import { useRef, useState } from 'react';
import { Input } from '~/components/ui/input';
import ProductSection from '../home/product_section';
import Glow from '~/components/common/glow';

export default function CartPage() {
  const slide1 = useRef<any>();

  return (
    <div className="relative">
      <div className="pt-24"></div>
      <div className="relative bg-background py-16 px-14 space-y-14">
        <h2 className="text-5xl font-bold uppercase">Basket</h2>
        <div data-name="cards" className="w-full border-b border-white/40">
          {[...Array(3)].map((_, i) => (
            <CartItem key={Math.random() * i} />
          ))}
        </div>
        <div className="bg-background space-y-4 w-1/3 ml-auto">
          <div className="flex bg-card border border-border">
            <Input
              placeholder="Coupon code"
              className="px-4 flex-1 bg-transparent border-none z-10 "
            />
            <Button
              variant={'ghost'}
              className="text-primary border-l border-border z-10 "
            >
              Apply Coupon
            </Button>
          </div>
          <div className="flex items-center justify-between z-10 ">
            <p className="text-lg">Total:</p>
            <p className="text-xl text-primary font-bold">AED: 240.00</p>
          </div>
          <Button
            variant={'clip'}
            size={'full'}
            className="uppercase text-lg font-bold z-10 "
          >
            Proceed to Checkout
          </Button>
        </div>
        <Glow className="absolute right-0 bottom-0 w-1/5 h-40 overflow-hidden -z-10" />
      </div>

      <div className="py-10 pl-14 pr-4">
        {/* 13 cards */}
        <ProductSection
          class="max-w-sm lg:max-w-sm"
          slidesToShow={3}
          center={false}
          title={'Last chance offer'}
          type="closing"
          slide={slide1}
        />
      </div>
      <Glow className="absolute right-0 bottom-0 w-1/5 h-60 overflow-hidden" />
    </div>
  );
}

function CartItem() {
  const [isSubscribe, setIsSubscribe] = useState(false);

  return (
    <div data-name="card" className="card py-8 border-t border-white/40">
      <div className="flex items-start gap-16">
        <div className="relative w-44 h-28">
          <Image
            src={CarImage}
            fill
            alt={'car image'}
            className="absolute border object-contain border-primary"
          />
          <div className="p-1 w-12 h-12 rounded-full overflow-hidden absolute top-[30%] -right-6 bg-gradient-to-b from-primary to-neutral-900">
            <Image
              src={BottleImage}
              alt={'car image'}
              className="w-full h-full object-cover  rounded-full bg-white"
            />
          </div>
        </div>
        <div className="py-4 flex items-start justify-between gap-16">
          <p className="text-2xl flex-1">
            Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash!
          </p>
          <div className="flex flex-col">
            <div className="flex items-start gap-8">
              <div className="bg-card flex items-center justify-between overflow-hidden ">
                <Button className="p-2 bg-primary text-background">
                  <i className="fas fa-minus text-2xl font-extrabold" />
                </Button>
                <p className="w-16 text-center text-xl">10</p>
                <Button className="p-2 bg-primary text-background">
                  <i className="fas fa-plus text-2xl font-extrabold" />
                </Button>
              </div>
              <p className="text-xl text-white font-bold">AED 120.00</p>
              <div className="space-y-2">
                <div className="flex items-center gap-3">
                  <p className={isSubscribe ? 'text-white' : 'text-white/40'}>
                    Subscription
                  </p>
                  <Switch
                    onClick={() => setIsSubscribe((prevValue) => !prevValue)}
                    checked={isSubscribe}
                  />
                </div>
              </div>
            </div>

            {isSubscribe ? (
              <div className="w-1/2 self-end grid grid-cols-2 gap-2">
                {['Weekly', 'Monthly', 'Quarterly', 'Update Cart'].map(
                  (frequency) => (
                    <div
                      key={frequency}
                      className="bg-card rounded-full p-2 flex gap-2"
                    >
                      {frequency}
                    </div>
                  ),
                )}
              </div>
            ) : null}
          </div>
          <i className="fas fa-times cursor-pointer text-white/40 text-sm border w-6 h-6 inline-flex items-center justify-center rounded-full" />
        </div>
      </div>
    </div>
  );
}
