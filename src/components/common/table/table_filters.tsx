import { useState } from 'react';
import { useDispatch } from 'react-redux';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { removeFromCart } from '~/store/reducers/cart';
import { trpc } from '~/utils/trpc';
import { LoginForm } from '../forms/form';
import {
  Sheet,
  SheetContent,
  SheetDescription,
  SheetFooter,
  SheetHeader,
  SheetOverlay,
  SheetTitle,
  SheetTrigger,
} from '~/components/ui/sheet';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '~/components/ui/form';
import { Input } from '~/components/ui/input';
import { Switch } from '~/components/ui/switch';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import { useForm } from 'react-hook-form';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';

interface SettingDialogInterface {
  inputList: object[];
  item_name: string;
  value: any;
  setValue: any;
  setFilters?: any;
  initial?: any;
}

export function TableFilters(props: SettingDialogInterface) {
  const { toast } = useToast();
  const [filter, setFilter] = useState(false);
  const [filterVal, setFilterVal] = useState<any>({});
  const [filterDate, setFilterDate] = useState<any>({});
  const [filterInput, setFilterInput] = useState<any>({});
  const form = useForm<any>({});

  const dispatch = useDispatch();
  const HandleFilterChange = (e: any, filter: string) => {
    const filterData = [
      'is_verified',
      'is_enabled',
      'is_percentage',
      'is_limit',
      'is_disabled',
      'is_limited'
    ];
    const data =
      filter == 'category_id' || e.target.value == 'delete'
        ? +e.target.value
        : filterData.includes(filter)
        ? e.target.value == 'true'
          ? true
          : false
        : e.target.value;

    if (data !== 'delete') {
      setFilterVal({
        ...filterVal,
        [filter]: data,
      });
    } else {
      setFilterVal((current: any) => {
        // remove cost key from object
        const { [filter]: data, ...rest } = current;
        return rest;
      });
    }
  };
  const filterInputsHandle = (filter: string, target: any) => {
    return setFilterInput({ ...filterInput, [filter]: target });
  };
  const filterDateHandler = (filter: string, target: any) => {
    if (filter === 'to') {
      return setFilterDate({ ...filterDate, [filter]: target });
    } else {
      return setFilterDate({ ...filterDate, [filter]: target });
    }
  };

  const handleDeleteFilter = () => {
    return (
      props.setValue({}),
      setFilter(!filter),
      setFilterVal({}),
      setFilterDate({}),
      setFilterInput({}),
      props?.setFilters &&
        props?.setFilters(
          props?.initial
            ? {
                ...props?.initial,
              }
            : {
                first: 0,
                rows: 10,
                lang_id: 1,
              },
        )
    );
  };
  const SubmitFilter = () => {
    return (
      props?.setValue({ ...filterVal, ...filterDate, ...filterInput }),
      setFilter(!filter),
      props?.setFilters &&
        props?.setFilters(
          props?.initial
            ? {
                ...props?.initial,
              }
            : {
                first: 0,
                rows: 10,
                lang_id: 1,
              },
        )
    );
  };

  const curr = new Date();
  curr.setDate(curr.getDate());
  const dateForInput = curr.toISOString().substring(0, 10);

  const today = new Date().toISOString().split('T')[0];
  async function onSubmit(values: any) {
    try {
      console.log({ values });
    } catch (error: any) {
      console.log(error?.message);
    }
  }
  return (
    <div>
      <Button
        variant="outline"
        className={`${
          Object.keys(props?.value)?.length ? ' text-primary' : ' text-white '
        } `}
        onClick={() => setFilter(!filter)}
      >
        Filters <i className={`fa-solid fa fa-filter   ml-2 h-4 w-4`}></i>
      </Button>
      <Sheet open={filter} onOpenChange={(e) => setFilter(e)}>
        <SheetOverlay className=" backdrop-blur-none" />
        <SheetContent
          className="w-80  border-border flex flex-col justify-between"
          side={'right'}
        >
          <SheetHeader>
            <SheetTitle>{props?.item_name} Filters</SheetTitle>
            <ScrollArea className="w-full h-[calc(100vh-165px)] ">
              <ScrollBar orientation="vertical"></ScrollBar>
              <Form {...form}>
                <form onSubmit={form.handleSubmit(onSubmit)}>
                  <SheetDescription className="pt-10 pr-3">
                    <div className=" grid grid-cols-1    items-center p-2">
                      {props?.inputList.map((item: any, i: number) => {
                        if (item?.type == 'text') {
                          return (
                            <div className="w-full" key={i}>
                              <FormItem className="w-full">
                                <FormLabel>{item?.text}</FormLabel>
                                <FormControl>
                                  <Input
                                    type={'text'}
                                    placeholder={item?.text}
                                    value={filterInput[item?.filtername]}
                                    onChange={(e) => {
                                      filterInputsHandle(
                                        item?.filtername,
                                        e.target.value,
                                      );
                                    }}
                                    // {...form.register(item.name)}
                                  />
                                </FormControl>

                                <div className="relative pb-2">
                                  <FormMessage />
                                </div>
                              </FormItem>
                            </div>
                          );
                        }
                        if (item?.type == 'number') {
                          return (
                            <div key={i}>
                              <FormItem>
                                <FormLabel>{item?.label}</FormLabel>
                                <FormControl>
                                  <Input
                                    type={'number'}
                                    defaultValue={1}
                                    min={1}
                                    onChange={(e) =>
                                      filterInputsHandle(
                                        item.filtername,
                                        e.target.value,
                                      )
                                    }
                                    placeholder={item?.placeholder}
                                  />
                                </FormControl>

                                <div className="relative pb-2">
                                  <FormMessage />
                                </div>
                              </FormItem>
                            </div>
                          );
                        }
                        if (item?.type == 'date') {
                          return (
                            <div key={i} className=" h-full">
                              <FormItem className=" flex flex-col gap-1  w-full">
                                <FormLabel>{item?.text}</FormLabel>
                                <FormControl>
                                  <Input
                                    type={'date'}
                                    placeholder={item?.text}
                                    onChange={(e) =>
                                      filterDateHandler(
                                        item?.filtername,
                                        e.target.value,
                                      )
                                    }
                                    value={filterDate[item?.filtername]}
                                  />
                                </FormControl>

                                <div className="relative pb-2">
                                  <FormMessage />
                                </div>
                              </FormItem>
                            </div>
                          );
                        }
                        if (item?.type == 'switch') {
                          return (
                            <div key={i} className=" h-full">
                              <FormItem className="flex items-center justify-between h-full  gap-2">
                                <FormLabel>
                                  Alternative Selling Option
                                </FormLabel>
                                <FormControl>
                                  {/* <Switch
                              checked={field.value}
                              onCheckedChange={field.onChange}
                            /> */}
                                </FormControl>

                                <div className="relative pb-2">
                                  <FormMessage />
                                </div>
                              </FormItem>
                            </div>
                          );
                        }
                        if (item?.type == 'select') {
                          return (
                            <div key={i}>
                              <FormItem>
                                <FormLabel>{item?.text}</FormLabel>
                                <select
                                  onChange={(e) =>
                                    HandleFilterChange(e, item.filtername)
                                  }
                                  value={
                                    filterVal[item.filtername]?.toString() ?? ''
                                  }
                                  className=" w-full px-2 py-3 border border-border bg-black focus:border-primary selection:border-border focus:ring-border"
                                >
                                  <option
                                    value={'delete'}
                                    //   selectOptions={'delete'}
                                  >
                                    Select {item.text}
                                  </option>
                                  {item?.filter?.map(
                                    (list: any, index: number) => {
                                      return (
                                        <option
                                          key={index}
                                          value={list?.id || list?.value}
                                          //   selectOptions={list?.id || list?.value}
                                        >
                                          {list?.name}
                                        </option>
                                      );
                                    },
                                  )}
                                </select>

                                <div className="relative pb-2">
                                  <FormMessage />
                                </div>
                              </FormItem>
                            </div>
                          );
                        } else {
                          return <div key={i}></div>;
                        }
                      })}
                    </div>
                  </SheetDescription>
                </form>
              </Form>
            </ScrollArea>
          </SheetHeader>
          <SheetFooter>
            <Button variant="outline" onClick={handleDeleteFilter}>
              Clear
            </Button>
            <Button
              variant="outline"
              className="bg-primary"
              onClick={SubmitFilter}
            >
              Apply
            </Button>
          </SheetFooter>
        </SheetContent>
      </Sheet>
    </div>
  );
}
