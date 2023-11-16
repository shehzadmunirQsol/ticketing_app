import React, { useEffect, useState } from 'react';
import {
  ColumnDef,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';

import { Button } from '@/ui/button';

import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import { LanguageInterface } from '../language_select';
import { trpc } from '~/utils/trpc';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from '../modal/loadingModal';
import { displayDate } from '~/utils/helper';
import Current from '~/public/assets/not-current-entrie.png';
import { useRouter } from 'next/router';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '~/components/ui/dropdown-menu';
import { MoreHorizontal } from 'lucide-react';
import { OrderViewDialog } from '../modal/orderView';
import { RootState } from '~/store/store';
import { useSelector } from 'react-redux';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import {
  DoubleArrowLeftIcon,
  DoubleArrowRightIcon,
} from '@radix-ui/react-icons';
import langContent from '~/locales';

import { ChevronLeftIcon, ChevronRightIcon } from '@radix-ui/react-icons';
import NextImage from '~/components/ui/img';

export type Category = {
  id: number;
  total_amount: number;
  discount_amount: number;
  sub_total_amount: number;
  created_at: Date;
};

interface OrderTableProps {
  filters: any;
  setFilters: any;
}

export default function OrdersDataByIdTable(props: OrderTableProps) {
  const { lang } = useSelector((state: RootState) => state.layout);
  const { user } = useSelector((state: RootState) => state.auth);
  const router = useRouter();

  const [sorting, setSorting] = useState<SortingState>([]);
  const [selectedItem, setSelectedItem] = useState({});
  const [title, setTitle] = useState('');
  const [type, setType] = useState('');
  const [isModal, setIsModal] = useState(false);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});

  const { data, isFetching } = trpc.order.getOrders.useQuery(
    { ...props.filters },
    {
      refetchOnWindowFocus: false,
      enabled: props?.filters.customer_id ? true : false,
    },
  );

  const orderData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);
  const handleView = (data: any, type: string) => {
    setSelectedItem(data);
    setTitle('Banner');
    setType(type);
    setIsModal(true);
  };

  useEffect(() => {
    props?.setFilters((prevFilters: any) => ({
      ...prevFilters,
      customer_id: user?.id,
    }));
  }, [user?.id]);

  const columns: ColumnDef<Category>[] = [
    {
      accessorKey: 'ID',
      header: 'Order ID',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          #{row?.original.id}
        </div>
      ),
    },
    {
      accessorKey: 'Sub Total',
      header: 'Sub Total',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          AED {(row?.original?.sub_total_amount).toFixed(2)}
        </div>
      ),
    },
    {
      accessorKey: 'Discount',
      header: 'Discount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {' '}
          {row?.original?.discount_amount > 0
            ? 'AED ' + (row?.original?.discount_amount).toFixed(2)
            : 'N/A'}
        </div>
      ),
    },
    {
      accessorKey: 'Total Amount',
      header: 'Total Amount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap w-28">
          {' '}
          {row?.original?.total_amount > 0
            ? 'AED ' + (row?.original?.total_amount).toFixed(2)
            : 'N/A'}
        </div>
      ),
    },
    {
      accessorKey: 'Created At',
      header: 'Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap overflow-hidden ">
          {displayDate(row?.original?.created_at)}
        </div>
      ),
    },
    {
      id: 'actions',
      header: 'Actions',
      enableHiding: false,
      cell: ({ row }) => {
        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuItem
                onClick={() => handleView(row?.original, 'view')}
              >
                View Order
              </DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];

  const table = useReactTable({
    data: orderData as Category[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function handlePagination(page: number) {
    if (page < 0) return;
    props?.setFilters((prevFilters: any) => ({ ...prevFilters, first: page }));
  }

  return (
    <div className="w-full p-2">
      {table?.getRowModel()?.rows?.length ? (
        <>
          <div className="rounded-md border border-border">
            <ScrollArea className="w-full ">
              <ScrollBar orientation="horizontal"></ScrollBar>
              <Table>
                <TableHeader className="bg-secondary/80">
                  {table?.getHeaderGroups().map((headerGroup) => (
                    <TableRow key={headerGroup.id}>
                      {headerGroup.headers.map((header) => {
                        return (
                          <TableHead key={header.id}>
                            {header.isPlaceholder
                              ? null
                              : flexRender(
                                  header.column.columnDef.header,
                                  header.getContext(),
                                )}
                          </TableHead>
                        );
                      })}
                    </TableRow>
                  ))}
                </TableHeader>
                <TableBody>
                  {table?.getRowModel()?.rows?.length ? (
                    table?.getRowModel()?.rows?.map((row) => (
                      <TableRow
                        key={row.id}
                        data-state={row.getIsSelected() && 'selected'}
                      >
                        {row.getVisibleCells().map((cell) => (
                          <TableCell key={cell.id}>
                            {flexRender(
                              cell.column.columnDef.cell,
                              cell.getContext(),
                            )}
                          </TableCell>
                        ))}
                      </TableRow>
                    ))
                  ) : (
                    <TableRow>
                      <TableCell
                        colSpan={columns.length}
                        className=" text-center"
                      >
                        <div className="flex flex-col my-auto h-full items-center justify-center">
                          <NextImage src={Current} alt="/" />
                          <p className="text-center text-gray-300 text-md my-2 px-6">
                            {
                              langContent[lang.lang].MyAccount.AccountView
                                .TABLE_INFO
                            }
                          </p>
                          <Button
                            variant={'rounded'}
                            className="text-center font-black tracking-tighter my-4 w-36 text-xs md:w-fit md:text-md "
                            onClick={() => router.push('/cars')}
                          >
                            {
                              langContent[lang.lang].MyAccount.AccountView
                                .TABLE_BUTTON
                            }
                          </Button>
                        </div>
                      </TableCell>
                    </TableRow>
                  )}
                </TableBody>
              </Table>
            </ScrollArea>
          </div>
          <div className="flex flex-col md:flex-row gap-2 items-center justify-center md:justify-end md:space-x-2 py-4">
            {data?.count && (
              <div className="flex-1 flex w-[100px] items-center justify-start text-sm font-medium">
                Page {props.filters.first + 1} of{' '}
                {Math.ceil(data?.count / props.filters.rows)}
              </div>
            )}
            <div className="flex flex-col md:flex-row items-center justify-center gap-2 md:space-x-6 lg:space-x-8">
              <div className="flex items-center space-x-2">
                <p className="text-sm font-medium">Rows per page</p>
                <Select
                  value={`${props.filters.rows}`}
                  onValueChange={(value) => {
                    props?.setFilters((prevFilters: any) => ({
                      ...prevFilters,
                      rows: Number(value),
                      first: 0,
                    }));
                    table.setPageSize(Number(value));
                  }}
                >
                  <SelectTrigger className="h-8 w-[70px]">
                    <SelectValue placeholder={props.filters.rows} />
                  </SelectTrigger>
                  <SelectContent side="top">
                    {[5, 10, 20, 30, 40, 50].map((pageSize) => (
                      <SelectItem key={pageSize} value={`${pageSize}`}>
                        {pageSize}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>

              <div className="flex items-center justify-center gap-2 md:space-x-2">
                <Button
                  variant="outline"
                  className="hidden h-8 w-8 p-0 lg:flex"
                  onClick={() => handlePagination(0)}
                  disabled={props.filters.first === 0}
                >
                  <span className="sr-only">Go to first page</span>
                  <DoubleArrowLeftIcon className="h-4 w-4" />
                </Button>
                <Button
                  variant="outline"
                  className="h-8 w-8 p-0"
                  onClick={() => handlePagination(props.filters.first - 1)}
                  disabled={props.filters.first === 0}
                >
                  <span className="sr-only">Go to previous page</span>
                  <ChevronLeftIcon className="h-4 w-4" />
                </Button>
                <Button
                  variant="outline"
                  className="h-8 w-8 p-0"
                  onClick={() => handlePagination(props.filters.first + 1)}
                  disabled={
                    (props.filters.first + 1) * props.filters.rows >
                      (data?.count || 0) ||
                    Math.ceil((data?.count ?? 0) / props.filters.rows) ==
                      props.filters.first + 1
                  }
                >
                  <span className="sr-only">Go to next page</span>
                  <ChevronRightIcon className="h-4 w-4" />
                </Button>
                <Button
                  variant="outline"
                  className="hidden h-8 w-8 p-0 lg:flex"
                  onClick={() =>
                    handlePagination(
                      Math.ceil((data?.count ?? 0) / props.filters.rows) - 1,
                    )
                  }
                  disabled={
                    (props.filters.first + 1) * props.filters.rows >
                      (data?.count || 0) ||
                    Math.ceil((data?.count ?? 0) / props.filters.rows) ==
                      props.filters.first + 1
                  }
                >
                  <span className="sr-only">Go to last page</span>
                  <DoubleArrowRightIcon className="h-4 w-4" />
                </Button>
              </div>
            </div>
          </div>
        </>
      ) : (
        <div className="flex flex-col my-auto h-full items-center justify-center">
          <NextImage src={Current} alt="/" />
          <p className="text-center text-gray-300 text-md my-2 px-6">
            {langContent[lang.lang].MyAccount.AccountView.TABLE_INFO}
          </p>
          <Button
            variant={'rounded'}
            className="text-center font-black tracking-tighter my-4 w-full h-fit text-xs sm:w-fit md:text-md "
            onClick={() => router.push('/cars')}
          >
            {langContent[lang.lang].MyAccount.AccountView.TABLE_BUTTON}
          </Button>
        </div>
      )}

      <OrderViewDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        type={type}
        setType={setType}
      />
      <LoadingDialog open={isFetching} text={'Loading data...'} />
    </div>
  );
}
