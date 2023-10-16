import React, { useState } from 'react';
import {
  ColumnDef,
  // ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown } from 'lucide-react';

import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';

import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';

import { trpc } from '~/utils/trpc';
import { customEmailTruncateHandler, displayDate } from '~/utils/helper';
import { getCustomerSchema } from '~/schema/customer';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '~/components/ui/tooltip';
import { Switch } from '~/components/ui/switch';
import { useToast } from '~/components/ui/use-toast';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { CouponDialog } from '../modal/coupon';
import { CouponDeleteDialog } from '../modal/deleteCoupon';
import { LoadingDialog } from '../modal/loadingModal';
import { MoreHorizontal } from 'lucide-react';
import Link from 'next/link';
import { TableFilters } from './table_filters';
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
import { ChevronLeftIcon, ChevronRightIcon } from '@radix-ui/react-icons';
import { useRouter } from 'next/router';
export type Category = {
  id: number;
  is_enabled: boolean;
  name: string;
  coupon_code: string;
  discount: number;
  coupon_limit: number;

  is_limited: boolean;
  is_percentage: boolean;
  created_at: Date;
  start_date: Date;
  end_date: Date;
  updated_at: Date;
};

export default function CouponsDataTable() {
  // use toast
  const { toast } = useToast();
  const router = useRouter();
  const { is_enabled } = router.query;
  console.log({ is_enabled });
  // use states
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filterID, setFilterID] = useState({});

  const [filters, setFilters] = useState<getCustomerSchema>({
    first: 0,
    rows: 10,
  });
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});
  const [selectedItem, setSelectedItem] = React.useState({});
  const [title, setTitle] = React.useState('');
  const [type, setType] = React.useState('');
  const [isModal, setIsModal] = React.useState(false);
  

  // APi
  const { data, refetch, isLoading } = trpc.coupon.get.useQuery(
    { ...filters, filters: { ...filterID } },
    {
      refetchOnWindowFocus: false,
    },
  );

  const categoryData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);

    // delete product
    const deleteCoupon = (data: any, type: string) => {
      setSelectedItem(data);
      setTitle('Coupon');
      setType(type);
      setIsModal(true);
    };
  


  // handle modal
  const handleEnbled = (data: any, type: string) => {
    console.log({ data, type }, 'enable check');
    if (!data?.is_approved) {
      setSelectedItem(data);
      setTitle('Coupon');
      setType(type);
      setIsModal(true);
    } else {
      toast({
        variant: 'disable',
        title: `Customer is Already Approved!`,
      });
    }
  };
  // columns
  const columns: ColumnDef<Category>[] = [
    {
      accessorKey: 'name',
      header: 'Name',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger>
                  {customEmailTruncateHandler(row?.original?.name)}
                </TooltipTrigger>
                <TooltipContent>
                  <p className="text-base font-normal">{row?.original?.name}</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
        );
      },
    },
    {
      accessorKey: 'Coupon Code',
      header: 'Coupon Code',
      cell: ({ row }) => (
        <div className="w-24 capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.coupon_code}
        </div>
      ),
    },
    {
      accessorKey: 'discount',
      header: 'Discount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {(row?.original?.discount).toFixed(2)}{' '}
          <sub>{row?.original?.is_percentage ? '%' : 'AED'}</sub>
        </div>
      ),
    },
    {
      accessorKey: 'Type',
      header: 'Type',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.is_percentage ? 'percentage' : 'fixed'}
        </div>
      ),
    },
    {
      accessorKey: 'Customer Limit',
      header: 'Customer Limit',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap w-32">
          {row?.original?.is_limited
            ? row?.original?.coupon_limit
            : 'unlimited'}
        </div>
      ),
    },
    {
      id: 'Enabled',
      header: 'Enabled',

      cell: ({ row }) => {
        return (
          <div>
            <Switch
              checked={row?.original?.is_enabled}
              onCheckedChange={() =>
                handleEnbled(
                  row?.original,
                  row?.original?.is_enabled ? 'enabled' : 'disabled',
                )
              }
            />
          </div>
        );
      },
    },
    {
      accessorKey: 'Start Date',
      header: 'Start Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {displayDate(row?.original?.start_date)}
        </div>
      ),
    },
    {
      accessorKey: 'End Date',
      header: 'End Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {displayDate(row?.original?.end_date)}
        </div>
      ),
    },
    {
      id: 'actions',
      enableHiding: false,
      header: 'Actions',
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
              {/* <DropdownMenuLabel>Actions</DropdownMenuLabel> */}
              {/* <DropdownMenuSeparator /> */}
              <Link href={`/admin/coupons/edit/${row?.original?.id}`}>
                <DropdownMenuItem>Edit</DropdownMenuItem>
              </Link>
              <DropdownMenuItem
                  onClick={() => deleteCoupon(row?.original, 'delete')}
                >
                  Delete
                </DropdownMenuItem>




            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];

  const table = useReactTable({
    data: categoryData as Category[],
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
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }

  // FILTER OPTIONS
  const roleOptions1 = [
    {
      Icon: 'fal fa-chevron-down',
      text: 'Search',
      filtername: 'searchQuery',
      type: 'text',
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'Enabled',
      filtername: 'is_enabled',
      type: 'select',

      filter: [
        {
          name: 'Yes',
          value: true,
        },
        {
          name: 'No',
          value: false,
        },
      ],
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Type',
      filtername: 'is_percentage',
      type: 'select',

      filter: [
        {
          name: 'Fixed',
          value: false,
        },
        {
          name: 'Percentage',
          value: true,
        },
      ],
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Limit',
      filtername: 'is_limited',
      type: 'select',

      filter: [
        {
          name: 'Limit',
          value: true,
        },
        {
          name: 'Unlimited',
          value: false,
        },
      ],
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Start Date',
      filtername: 'startDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'End Date',
      filtername: 'endDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Clear Filter',
      filtername: 'Clear',
    },
  ];

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-end gap-2">
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
        <TableFilters
          inputList={roleOptions1}
          item_name={'Coupon'}
          value={filterID}
          setValue={setFilterID}
          setFilters={setFilters}
        />
      </div>
      <div className="rounded-md border border-border ">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>

          <Table className="w-full overflow-x-scroll">
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
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="flex-1 flex w-[100px] items-center justify-start text-sm font-medium">
          Page {filters.first + 1} of{' '}
          {Math.ceil((data?.count ?? 0) / filters.rows)}
        </div>

        <div className="flex items-center justify-center space-x-6 lg:space-x-8">
          <div className="flex items-center space-x-2">
            <p className="text-sm font-medium">Rows per page</p>
            <Select
              value={`${filters.rows}`}
              onValueChange={(value) => {
                setFilters((prevFilters: any) => ({
                  ...prevFilters,
                  rows: Number(value),
                  first: 0,
                }));
                table.setPageSize(Number(value));
              }}
            >
              <SelectTrigger className="h-8 w-[70px]">
                <SelectValue placeholder={filters.rows} />
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

          <div className="flex items-center space-x-2">
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => handlePagination(0)}
              disabled={filters.first === 0}
            >
              <span className="sr-only">Go to first page</span>
              <DoubleArrowLeftIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters?.first - 1)}
              disabled={filters?.first === 0}
            >
              <span className="sr-only">Go to previous page</span>
              <ChevronLeftIcon className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters.first + 1)}
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
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
                  Math.ceil((data?.count ?? 0) / filters.rows) - 1,
                )
              }
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to last page</span>
              <DoubleArrowRightIcon className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </div>
      <CouponDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        refetch={refetch}
        type={type}
        setType={setType}
      />
      <CouponDeleteDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        refetch={refetch}
        type={type}
        setType={setType}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
